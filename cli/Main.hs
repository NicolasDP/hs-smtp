-- |
-- Module      :
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prina.fr>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.List (intercalate)
import Network.SMTP

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
        []       -> usage (Just "need at least sender and recipient email address")
        ["help"] -> usage Nothing
        "verify":fromStr:rcptStr:[] -> mainVerify fromStr rcptStr >>= putStrLn . prettyPrintResult "  "
        _        -> usage (Just $ "unrecognized options: " ++ intercalate " " args)


mainVerify :: String -> String -> IO (Result ())
mainVerify fromStr rcptStr = executeSMTP $ do
    mxs <- lookupMXs (domainpart rcpt)
    case mxs of
        []     -> fail "no MX server for the given recipient"
        (mx:_) -> do
            con <- openSMTPConnection (mxAddress mx) 25 (domainpart from)
            mailFrom con from
            bool <- rcptTo con rcpt
            when (not bool) $ fail $ "cannot send email to " ++ rcptStr
            closeSMTPConnection con
  where
    from :: EmailAddress
    from = either error id $ emailAddrFromString fromStr
    rcpt :: EmailAddress
    rcpt = either error id $ emailAddrFromString rcptStr

usage :: Maybe String
      -> IO ()
usage mMsg = printUsage $ intercalate "\n"
    [ maybe "SMTP Client Command Line interface" ((++) "Error: ") mMsg
    , ""
    , "available options:"
    , "  help: show this help message"
    ]
  where
    printUsage :: String -> IO ()
    printUsage = case mMsg of
        Nothing -> putStrLn
        Just _  -> error
