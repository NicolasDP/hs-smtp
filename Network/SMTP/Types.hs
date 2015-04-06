-- |
-- Module      : Network.SMTP.Types
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE OverloadedStrings #-}

module Network.SMTP.Types
    ( -- * Types
      Domain(..)
    , LocalPart(..)
    , EmailAddress(..)
    , Path(..)
    , ReversePath(..)
    , ForwardPath(..)
      -- * Authentification
    , UserName
    , Password
    , AuthType(..)
    , showAuthType
    , readAuthType
      -- * SMTP Commands
      -- ** Command
    , Command(..)
    , showCommand
      -- ** SMTP extensions
    , ESMTPKeyWord
    , ESMTPValue
    , ESMTPParameters
    , emptyESMTPParameters
    , MailParameters
    , RcptParameters
      -- * SMTP Responses
    , Response(..)
    , ResponseCode(..)
    , responseCodeToInt
    , responseCodeFromInt
    ) where

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Domain = Domain
    { getDomain :: ByteString
    } deriving (Eq, Show)
instance Byteable Domain where
    toBytes = getDomain
newtype LocalPart = LocalPart ByteString
  deriving (Eq, Show)
instance Byteable LocalPart where
    toBytes (LocalPart l) = l

-- | Email address
--
-- > localpart@domain.net
data EmailAddress = EmailAddress
    { localpart  :: !LocalPart
    , domainpart :: !Domain
    } deriving (Eq, Show)

instance Byteable EmailAddress where
    toBytes addr = BC.concat [ lpart, "@", dpart ]
      where
        lpart = toBytes $ localpart addr
        dpart = toBytes $ domainpart addr

-- | RFC5321 (4.1.2) specified a Path is a list of domains and an email address
data Path = Path
    { paths   :: ![Domain]     -- ^ might be useless since it is deprecated
    , address :: !EmailAddress
    } deriving (Show, Eq)

instance Byteable Path where
    toBytes (Path adl from) = BC.concat ["<", showADL, toBytes from, ">"]
      where
        showADL :: ByteString
        showADL = showADL' adl

        showADL' :: [Domain] -> ByteString
        showADL' []               = BC.empty
        showADL' [Domain ad]      = BC.concat ["@", ad, ":"]
        showADL' ((Domain ad):xs) = BC.concat ["@", ad, ",", showADL' xs]

-- | As specified in RFC5321 (4.1.2), a reverse path is a Path
newtype ReversePath = ReversePath (Maybe Path)
  deriving (Eq, Show)
instance Byteable ReversePath where
    toBytes (ReversePath Nothing)  = "<>"
    toBytes (ReversePath (Just p)) = toBytes p

-- | As specified in RFC5321 (4.1.2), a forward path is a Path
newtype ForwardPath = ForwardPath Path
  deriving (Eq, Show)
instance Byteable ForwardPath where
    toBytes (ForwardPath p) = toBytes p

------------------------------------------------------------------------------
--                            Authentifications                             --
------------------------------------------------------------------------------

-- | user's name
type UserName = ByteString

-- | user's digest
type Password = ByteString

-- | type of authentification implemented
data AuthType
    = PLAIN
    | LOGIN
    | CRAM_MD5
  deriving (Show, Eq, Enum, Ord)

showAuthType :: AuthType -> ByteString
showAuthType t = case t of
    PLAIN    -> "PLAIN"
    LOGIN    -> "LOGIN"
    CRAM_MD5 -> "CRAM-MD5"
instance Byteable AuthType where
    toBytes = showAuthType

readAuthType :: ByteString -> Either String AuthType
readAuthType bs = case BC.map toUpper bs of
    "PLAIN"    -> Right PLAIN
    "LOGIN"    -> Right LOGIN
    "CRAM-MD5" -> Right CRAM_MD5
    _          -> Left $ "Authentication method not supported: " ++ show bs

------------------------------------------------------------------------------
--                               Commands                                   --
------------------------------------------------------------------------------

-- | A ESMTP key word
type ESMTPKeyWord = ByteString

-- | A ESMTPValue
type ESMTPValue   = ByteString

-- | A ESMTPParameter collection
type ESMTPParameters = Map ESMTPKeyWord (Maybe ESMTPValue)

-- | the MAIL command may receive parameters
type MailParameters = ESMTPParameters
-- | the RCTP command may receive parameters
type RcptParameters = ESMTPParameters

emptyESMTPParameters :: ESMTPParameters
emptyESMTPParameters = Map.empty

-- | (E)SMTP commands
data Command
    = HELO Domain
    | EHLO Domain
    | MAIL ReversePath MailParameters
    | RCPT ForwardPath RcptParameters
    | DATA
    | EXPN ByteString
    | VRFY ByteString
    | HELP (Maybe ByteString)
    | AUTH AuthType (Maybe ByteString)
    | NOOP (Maybe ByteString)
    | RSET
    | QUIT
    | STARTTLS
    deriving (Eq, Show)

showCommand :: Command -> ByteString
showCommand cmd = BC.intercalate " " $
  case cmd of
    HELO (Domain dom) -> ["HELO", dom]
    EHLO (Domain dom) -> ["EHLO", dom]
    MAIL rp mp        -> ["MAIL", BC.append "FROM:" $ toBytes rp] ++ (showParameters' $ Map.toList mp)
    RCPT rp mp        -> ["RCPT", BC.append "TO:"   $ toBytes rp] ++ (showParameters' $ Map.toList mp)
    DATA              -> ["DATA"]
    EXPN expn         -> ["EXPN", expn]
    VRFY str          -> ["VRFY", str]
    HELP mcmd         -> ("HELP"): maybe [] (flip (:) []) mcmd
    AUTH t ms         -> ("AUTH"): toBytes t : maybe [] (flip (:) []) ms
    NOOP ms           -> ("NOOP"): maybe [] (flip (:) []) ms
    RSET              -> ["RSET"]
    QUIT              -> ["QUIT"]
    STARTTLS          -> ["STARTTLS"]
  where
    showParameters' :: [(ByteString, Maybe ByteString)] -> [ByteString]
    showParameters' xs = map showParameter xs

    showParameter :: (ByteString, Maybe ByteString) -> ByteString
    showParameter (k, mv) =
      case mv of
        Just v -> BC.concat [k, "=", v]
        Nothing -> k

instance Byteable Command where
    toBytes c = BC.append (showCommand c) "\r\n"

------------------------------------------------------------------------------
--                               Response                                   --
------------------------------------------------------------------------------

-- | Non-exautive list of possible response code
data ResponseCode
    = RC500SyntaxCommandError
    | RC501SyntaxParameterError
    | RC502CommandNotImplemented
    | RC503BadSequenceOfCommand
    | RC504ParameterNotImplemented
    | RC211SystemStatus
    | RC214Help
    | RC220ServiceReady
    | RC221ServiceClosingChannel
    | RC421ServiceNotAvailable
    | RC250Ok
    | RC251UserNotLocalButTry
    | RC252CannotVRFYUser
    | RC455ServerUnableToAccParrameters
    | RC555FromOrToError
    | RC450MailActionRejectedMailboxUnavailable
    | RC550ActionRejectedMailboxUnavailable
    | RC451ActionAborted
    | RC551UserNotLocalFail
    | RC452ActionRejectedSystemStorage
    | RC552MailActionAbbortedSystemStorage
    | RC553ActionRejectedMailboxNotAllowed
    | RC354StartMailInput
    | RC554Failed
    | RCUnknown Int
	deriving (Show, Eq)
instance Byteable ResponseCode where
    toBytes = BC.pack . show . responseCodeToInt

responseCodeList :: [(Int, ResponseCode)]
responseCodeList =
    [ (500, RC500SyntaxCommandError)
    , (501, RC501SyntaxParameterError)
    , (502, RC502CommandNotImplemented)
    , (503, RC503BadSequenceOfCommand)
    , (504, RC504ParameterNotImplemented)
    , (211, RC211SystemStatus)
    , (214, RC214Help)
    , (220, RC220ServiceReady)
    , (221, RC221ServiceClosingChannel)
    , (421, RC421ServiceNotAvailable)
    , (250, RC250Ok)
    , (251, RC251UserNotLocalButTry)
    , (252, RC252CannotVRFYUser)
    , (455, RC455ServerUnableToAccParrameters)
    , (555, RC555FromOrToError)
    , (450, RC450MailActionRejectedMailboxUnavailable)
    , (550, RC550ActionRejectedMailboxUnavailable)
    , (451, RC451ActionAborted)
    , (551, RC551UserNotLocalFail)
    , (452, RC452ActionRejectedSystemStorage)
    , (552, RC552MailActionAbbortedSystemStorage)
    , (553, RC553ActionRejectedMailboxNotAllowed)
    , (354, RC354StartMailInput)
    , (554, RC554Failed)
    ]

responseCodeFromInt :: Int -> ResponseCode
responseCodeFromInt rcode =
    case L.lookup rcode responseCodeList of
        Just c  -> c
        Nothing -> RCUnknown rcode

responseCodeToInt :: ResponseCode -> Int
responseCodeToInt rc = case rc of
    RCUnknown rcode -> rcode
    _               -> case L.find (\(_, rc') -> rc' == rc) responseCodeList of
        Nothing         -> 554
        Just (rcode, _) -> rcode

-- | As described in a RFC5321, a response is a CODE with message
-- see section 4.2.1
data Response = Response
    { code    :: !ResponseCode
    , message :: ![ByteString]
    } deriving (Eq, Show)

showResponse :: Response -> ByteString
showResponse r = showResp $ message r
  where
    codeStr :: ByteString
    codeStr = toBytes $ code r
    showResp :: [ByteString] -> ByteString
    showResp []     = BC.concat [codeStr, "\r\n"]
    showResp [msg]  = BC.concat [codeStr, " ", msg, "\r\n"]
    showResp (x:xs) = BC.concat [codeStr, "-", x, "\r\n", showResp xs]
instance Byteable Response where
    toBytes = showResponse
