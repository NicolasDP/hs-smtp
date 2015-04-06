-- |
-- Module      :
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

module Network.SMTP
    ( module Network.SMTP.Types
    , module Network.SMTP.Monad
    -- * Client
    , module Network.SMTP.Client

    -- * Utils
    -- ** Lookup MX domains
    , MXServer(..)
    , lookupMXs
    -- ** email address
    , emailAddrFromString
    -- ** pretty print result
    , prettyPrintResult
    ) where

import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Parse as BP (parse, Result(..))
import Data.List (intercalate, sort)
import Data.Hourglass
import qualified Network.DNS as DNS

import Network.SMTP.Types
import Network.SMTP.Monad
import Network.SMTP.Client
import Network.SMTP.Parser

data MXServer = MXServer
    { mxWeight  :: Int
    , mxAddress :: Domain
    } deriving (Eq, Show)

instance Ord MXServer where
    compare mx1 mx2 = compare (mxWeight mx1) (mxWeight mx2)

showMXServer :: MXServer -> String
showMXServer (MXServer w d) = "(" ++ show w ++ ")" ++ BC.unpack (toBytes d)

-- | This function retrieve the MX Server which are reponsible for the given
-- email address
lookupMXs :: Domain -> SMTP [MXServer]
lookupMXs domain = do
    seed <- smtpLiftIO "get the DNS Resolver information" $ DNS.makeResolvSeed resolvconf
    eres <- smtpLiftIO "send the DNS MXs Query" $ DNS.withResolver seed $ \r -> DNS.lookupMX r (toBytes domain)
    case eres of
        Left err -> fail $ "unable to collect the MX Domains for " ++ BC.unpack (toBytes domain) ++ " error is: " ++ show err
        Right v  -> do
            let mxs = sort $ map (\(d, p) -> MXServer p (Domain d)) v
            smtpReportInfo $ ("collecting MX for " ++ BC.unpack (toBytes domain)) ++ ": " ++ (intercalate " " $ map showMXServer mxs)
            return mxs
  where
    resolvconf :: DNS.ResolvConf
    resolvconf = DNS.defaultResolvConf


emailAddrFromString :: String -> Either String EmailAddress
emailAddrFromString str = case BP.parse parseEmailAddress (BC.snoc bs '\0') of
    BP.ParseFail err -> Left $ "failed to parse: " ++ show bs ++ " " ++ err
    BP.ParseMore _   -> Left $ "failed to parse: " ++ show bs ++ " not enough bytes"
    BP.ParseOK _ v   -> Right v
  where
    bs :: ByteString
    bs = BC.pack str

prettyPrintResult :: String
                  -> Result ()
                  -> String
prettyPrintResult indentation rs =
    let (report, header) = case rs of
            SMTPOK r _ -> (r, "Success")
            SMTPKO r   -> (r, "Failure")
    in indentation ++ header ++ "\n" ++ (prettyPrintReports (indentation ++ "  ") report)

prettyPrintReports :: String
                   -> SMTPReports 
                   -> String
prettyPrintReports indentation rs = intercalate "\n" $
    map (prettyPrintReport indentation) (reverse $ smtpReports rs)

prettyPrintReport :: String
                  -> SMTPReport
                  -> String
prettyPrintReport indentation r = case r of
    SMTPReportInfo     {} -> prettyPrintString   indentation time "INF" (smtpReportReport r)
    SMTPReportError    {} -> prettyPrintString   indentation time "ERR" (smtpReportReport r)
    SMTPReportCommand  {} -> prettyPrintCommand  indentation time       (smtpReportCmd r)
    SMTPReportResponse {} -> prettyPrintResponse indentation time       (smtpReportRespCode r)
  where
    time :: String
    time = timePrint "EPOCH.p6" $ smtpReportDate r

prettyPrintString :: String -> String -> String -> String -> String
prettyPrintString indentation time ty msg =
    indentation ++ "[" ++ time ++ "][" ++ ty ++ "] " ++ msg

prettyPrintCommand :: String -> String -> Command -> String
prettyPrintCommand indentation time cmd =
    indentation ++ "[" ++ time ++ "][CMD] " ++ (BC.unpack $ showCommand cmd)

prettyPrintResponse :: String
                    -> String
                    -> Response
                    -> String
prettyPrintResponse indentation time resp =
    intercalate "\n" $ map (((++) prefix) . BC.unpack) $ message resp
  where
    prefix :: String
    prefix = indentation ++ "[" ++ time ++ "][RSP][" ++ show (responseCodeToInt $ code resp) ++ "] "
