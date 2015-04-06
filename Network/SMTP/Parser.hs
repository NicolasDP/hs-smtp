-- |
-- Module      : Network.SMTP.Parser
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SMTP.Parser
    ( parseCommandByteString
    , parseCommandString
    , parseResponseByteString
    , parseResponseString
    
      -- * Parsers
    , parseDomainPart
    , parseEmailAddress
    , parseForwardPath
    , parseReversePath
    , parseCommand
    , parseResponse
    ) where

import Network.SMTP.Types

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BI
import Data.ByteString.Parse (Parser)
import qualified Data.ByteString.Parse as BP
import Data.Char (toUpper, isAlphaNum, isControl, isSpace)
import qualified Data.Map.Strict as Map

-- BSParse helpers ------------------------------------------------------------

char :: Char -> Parser Char
char c = (BP.byte $ BI.c2w c) >> return c

string :: String -> Parser ()
string str = mapM char str >> return ()

parserTakeWhile :: (Char -> Bool) -> Parser ByteString
parserTakeWhile test = BP.takeWhile (test . BI.w2c)

parseSome :: Parser a -> Parser [a]
parseSome parser = parseTail <|> return []
  where
    parseTail = do
        h <- parser
        hs <- parseSome parser
        return $ h : hs

satisfy :: String -> (Char -> Bool) -> Parser Char
satisfy errMsg test = do
    c <- BI.w2c <$> BP.anyByte
    if test c
        then return c
        else fail errMsg

-------------------------------------------------------------------------------
--                      Parse Email Address and Email Path                   --
-------------------------------------------------------------------------------

-- Parse LocalPart ------------------------------------------------------------

isAText :: Char -> Bool
isAText c
    =  c `elem` ['a'..'z']
    || c `elem` ['A'..'Z']
    || c `elem` "!#$%&'*+-/=?^_`{|}~"

parseAtom :: Parser ByteString
parseAtom = parserTakeWhile isAText

parseDotString :: Parser ByteString
parseDotString = do
    str  <- parseAtom
    lstr <- parseSome (BC.cons <$> char '.' <*> parseAtom)
    return $ BC.concat $ str : lstr

isAQuotedText :: Char -> Bool
isAQuotedText c
    | c == ' ' = True
    | c == '!' = True
    | n >= 35 && n <= 91 = True
    | n >= 93 && n <= 126 = True
    | otherwise = False
  where
    n = BI.c2w c

parseQuotedString :: Parser ByteString
parseQuotedString = do
    _ <- char '"'
    str <- parserTakeWhile isAQuotedText
    _ <- char '"'
    return $ BC.cons '"' $ BC.snoc str '"'

parseLocalPart :: Parser LocalPart
parseLocalPart = LocalPart <$> (parseDotString <|> parseQuotedString)

-- Parse DomainPart -----------------------------------------------------------

isLetterOrDigit :: Char -> Bool
isLetterOrDigit c
    =  c `elem` ['a'..'z']
    || c `elem` ['A'..'Z']
    || c `elem` ['0'..'9']

parseLdhStr :: Parser ByteString
parseLdhStr = do
    str <- parserTakeWhile (\c -> isLetterOrDigit c || c == '-')
    when (not (BC.null str) && BC.last str == '-') $
        fail "a domain node can't end with dash '-'"
    return str

parseSubDomain :: Parser ByteString
parseSubDomain = do
    node <- BC.cons
                <$> satisfy "a domain node cannot start with a dash '-'" isLetterOrDigit
                <*> parseLdhStr
    when (BC.length node > 63) $
        fail "a domain node can't be longer than 63 bytes"
    return node

parseDomain :: Parser Domain
parseDomain = do
    sdom <- parseSubDomain
    lsdom <- parseSome (BC.cons <$> char '.' <*> parseSubDomain)
    let dom = BC.concat $ sdom : lsdom
    when (BC.length dom > 255) $
        fail "email address's domain part is too long"
    return $ Domain dom

parseDomainPart :: Parser Domain
parseDomainPart =
    parseDomain -- <|> parseAddressLitteral

-- Parse EmailAddress ---------------------------------------------------------

parseEmailAddress :: Parser EmailAddress
parseEmailAddress = do
    lpart <- parseLocalPart
    _ <- char '@'
    dpart <- parseDomainPart
    return $ EmailAddress
        { localpart  = lpart
        , domainpart = dpart
        }

-------------------------------------------------------------------------------
--                              Path Parser                                  --
-------------------------------------------------------------------------------

parseAtDomainList :: Parser [Domain]
parseAtDomainList = do
    h  <- (char '@' >> (flip (:) [] <$> parseDomainPart)) <|> (return [])
    hs <- parseSome (char ',' >> char '@' >> parseDomainPart)
    let l = h ++ hs
    when (not $ null l) $ (void $ char ':')
    return l

parsePath :: Parser Path
parsePath = do
    _ <- char '<'
    list <- parseAtDomainList
    mail <- parseEmailAddress
    _ <- char '>'
    return $ Path list mail

-- | RFC5321 (4.1.2) says a ReversePath is a Path or empty.
parseReversePath :: Parser ReversePath
parseReversePath = ReversePath <$>
    ((Just <$> parsePath) <|> (char '<' >> char '>' >> return Nothing))

-- | a forward path is a Path
parseForwardPath :: Parser ForwardPath
parseForwardPath = ForwardPath <$> parsePath

------------------------------------------------------------------------------
--                               Extension parameters                       --
------------------------------------------------------------------------------

parseMailParameters :: Parser MailParameters
parseMailParameters = Map.delete "" <$> parseESMTPParameters

parseRcptParameters :: Parser RcptParameters
parseRcptParameters = Map.delete "" <$> parseESMTPParameters

parseESMTPParameters :: Parser ESMTPParameters
parseESMTPParameters = do
    mpair <- parseESMTPParameter
    list  <- do parseSP
                parseESMTPParameters
             <|> return Map.empty
    return $ case mpair of
                Just (k, v) -> Map.insert k v list
                Nothing     -> list

parseESMTPParameter :: Parser (Maybe (ESMTPKeyWord, Maybe ESMTPValue))
parseESMTPParameter =
    do key <- parseESMTPKeyWord
       mvalue <- do _ <- char '='
                    value <- parseESMTPValue
                    return $ Just value
                 <|> return Nothing
       return $ Just (key, mvalue)
    <|> return Nothing

parseESMTPKeyWord :: Parser ESMTPKeyWord
parseESMTPKeyWord = parserTakeWhile $ \c -> isAlphaNum c || c == '-'

parseESMTPValue :: Parser ESMTPValue
parseESMTPValue =
    parserTakeWhile $ \c -> not (isControl c || c == '=' || isSpace c)

------------------------------------------------------------------------------
--                               Strings and specials                       --
------------------------------------------------------------------------------

parseParameterString :: Parser ByteString
parseParameterString = do
    parseSP
    param <- parserTakeWhile $ \c -> isAlphaNum c || c == '.' || c == '-'
    return param

parseParameterMaybeString :: Parser (Maybe ByteString)
parseParameterMaybeString =
    do  s <- parseParameterString
        return $ Just s
    <|> return Nothing

-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
-- TODO: this is not base64: FIXME
parseParameterB64String :: Parser ByteString
parseParameterB64String = do
    parseSP
    param <- parserTakeWhile $ \c -> isAlphaNum c || c == '=' || c == '.' || c == '-'
    return param

parseParameterB64MaybeString :: Parser (Maybe ByteString)
parseParameterB64MaybeString =
    do  s <- parseParameterB64String
        return $ Just s
    <|> return Nothing

parseSP :: Parser ()
parseSP = char ' ' >> return ()

parseCRLF :: Parser ()
parseCRLF = parseCR >> parseLF >> return ()

parseCR :: Parser Char
parseCR = char '\r'

parseLF :: Parser Char
parseLF = char '\n'

------------------------------------------------------------------------------
--                               Command                                    --
------------------------------------------------------------------------------

parseCommandHELO :: Parser Command
parseCommandHELO = do
    parseSP
    domain <- parseDomain
    return $ HELO domain

parseCommandEHLO :: Parser Command
parseCommandEHLO = do
    parseSP
    domain <- parseDomain
    return $ EHLO domain

parseCommandMAIL :: Parser Command
parseCommandMAIL = do
    parseSP
    string "FROM:"
    rpath <- parseReversePath
    eparams <- parseMailParameters
    return $ MAIL rpath eparams

parseCommandRCPT :: Parser Command
parseCommandRCPT = do
    parseSP
    string "TO:"
    fpath   <- parseForwardPath
    eparams <- parseRcptParameters
    return $ RCPT fpath eparams

parseCommandEXPN :: Parser Command
parseCommandEXPN = do
    param <- parseParameterString
    return $ EXPN param

parseCommandVRFY :: Parser Command
parseCommandVRFY = do
    param <- parseParameterString
    return $ VRFY param

parseCommandNOOP :: Parser Command
parseCommandNOOP = do
    mparam <- parseParameterMaybeString
    return $ NOOP mparam

parseParameterAuthType :: Parser AuthType
parseParameterAuthType = do
    param <- parserTakeWhile $ \c -> isAlphaNum c || c == '.' || c == '-'
    return $ case BC.map toUpper param of
                "PLAIN"    -> PLAIN
                "LOGIN"    -> LOGIN
                "CRAM-MD5" -> CRAM_MD5
                _          -> error $ "not supported auth type: " ++ show param

parseCommandAUTH :: Parser Command
parseCommandAUTH = do
    parseSP
    authType <- parseParameterAuthType
    param <- parseParameterB64MaybeString
    return $ AUTH authType param

parseCommandHELP :: Parser Command
parseCommandHELP = do
    mparam <- parseParameterMaybeString
    return $ HELP mparam

parseCommand :: Parser Command
parseCommand = do
    cmd <- parserTakeWhile (\c -> c `elem` (['a'..'z'] ++ ['A'..'Z']))
    c <- case BC.map toUpper cmd of
        "HELO" -> parseCommandHELO
        "EHLO" -> parseCommandEHLO
        "MAIL" -> parseCommandMAIL
        "RCPT" -> parseCommandRCPT
        "DATA" -> return DATA
        "EXPN" -> parseCommandEXPN
        "VRFY" -> parseCommandVRFY
        "HELP" -> parseCommandHELP
        "AUTH" -> parseCommandAUTH
        "NOOP" -> parseCommandNOOP
        "RSET" -> return RSET
        "QUIT" -> return QUIT
        "STARTTLS" -> return STARTTLS
        _      -> fail $ "command not supported: " ++ show cmd
    parseCRLF
    return c

-- | Parses a String
parseCommandString :: String -> Either String Command
parseCommandString s = parseCommandByteString $ BC.pack s

-- | Parses a ByteString
--
-- also expect the CRLF at the end of the command
parseCommandByteString :: ByteString -> Either String Command
parseCommandByteString bs =
    case BP.parse parseCommand bs of
        BP.ParseFail err -> Left err
        BP.ParseMore _   -> Left "not enough bytes"
        BP.ParseOK _ v   -> Right v

------------------------------------------------------------------------------
--                               Response                                   --
------------------------------------------------------------------------------

parseResponseCode :: Parser ResponseCode
parseResponseCode =
    responseCodeFromInt . read . BC.unpack <$> BP.take 3

parseResponseFinalLine :: Parser Response
parseResponseFinalLine = do
    rcode <- parseResponseCode
    l <- (char ' ' >> parserTakeWhile ((/=) '\r') >>= return . flip (:) []) <|> return []
    parseCRLF
    return $ Response rcode l

parseResponseInterLine :: Parser ByteString
parseResponseInterLine = do
    _  <- parseResponseCode
    _  <- char '-'
    bs <- parserTakeWhile ((/=) '\r')
    parseCRLF
    return bs

parseResponse :: Parser Response
parseResponse
    = parseResponseFinalLine
    <|> do
      r <- parseResponseInterLine
      rs <- parseResponse
      return $ rs { message = r : message rs }

-- | parses a response String
parseResponseString :: String -> Either String Response
parseResponseString s = parseResponseByteString $ BC.pack s

-- | parses a response ByteString
parseResponseByteString :: ByteString -> Either String Response
parseResponseByteString bs =
    case BP.parse parseResponse bs of
        BP.ParseFail err -> Left err
        BP.ParseMore _   -> Left "not enough bytes"
        BP.ParseOK _ v   -> Right v
