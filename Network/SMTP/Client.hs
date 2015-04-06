-- |
-- Module      :
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE OverloadedStrings #-}

module Network.SMTP.Client
    ( openSMTPConnection
    , closeSMTPConnection
    , mailFrom
    , rcptTo
    , withSendData
    , startSendData
    , stopSendData
    -- * Low level
    , sendSMTPCommand
    , sendSMTPCommandWith
    , doesResponseContainsLine
    ) where

import Control.Monad
import Data.Byteable
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.Char (toUpper)
import Data.Default.Class
import Network.Connection

import Network.SMTP.Types
import Network.SMTP.Monad
import Network.SMTP.Parser

-- | attempts to open a connection to the given Domain at the given port number
--
-- also tries to Start TLS if available
openSMTPConnection :: Domain -- ^ the SMTP server to contact
                   -> Int    -- ^ the port number (port 25 for most of the time)
                   -> Domain -- ^ the domain to send our Cheering from (HELO/EHLO)
                   -> SMTP Connection
openSMTPConnection dom port domain = do
    ctx <- smtpLiftIO "Init connection context"   initConnectionContext
    con <- smtpLiftIO "Open connection to Domain" $ connectTo ctx $ ConnectionParams
        { connectionHostname  = BC.unpack $ getDomain dom
        , connectionPort      = fromIntegral port
        , connectionUseSecure = Nothing
        , connectionUseSocks  = Nothing
        }
    smtpReportInfo $ "Connection established to domain: " ++ BC.unpack (getDomain dom) ++ ":" ++ show port
    -- Received the Server Greetings
    _ <- recvSMTPResponse con
    -- Send the client Hello
    ehloResp <- sendSMTPCommandWith con (EHLO domain) [ RC250Ok ] 
    -- enable TLS if supported:
    when (doesResponseContainsLine ehloResp "STARTTLS") $ do
        -- Send the StartTLS Command
        _ <- sendSMTPCommandWith con STARTTLS [ RC220ServiceReady ]
        smtpLiftIO "set Connection TLS" $ connectionSetSecure ctx con tlssettings
        -- reset the connection state
        _ <- sendSMTPCommandWith con (EHLO domain) [ RC250Ok ]
        return ()
    return con
  where
    -- TODO enable sertificate Validation
    tlssettings :: TLSSettings
    tlssettings = def
        { settingDisableCertificateValidation = True
        }

-- | properly closes the given connection
closeSMTPConnection :: Connection -> SMTP ()
closeSMTPConnection con = do
    _ <- sendSMTPCommandWith con QUIT [ RC221ServiceClosingChannel ]
    return ()

-- | Send the email address of the sender of the email
mailFrom :: Connection
         -> EmailAddress
         -> SMTP ()
mailFrom con emailAddr = do
    _ <- sendSMTPCommandWith con (MAIL rPath emptyESMTPParameters) [ RC250Ok ]
    return ()
  where
    rPath :: ReversePath
    rPath = ReversePath $ Just $ Path
        { paths = []
        , address = emailAddr
        }

-- | attempt to send a recipient of the email.
--
-- the command can:
--
-- * returns True if the command succeeded
--
-- * returns False if the Mail Bos is not allowed or unavailable
--
-- * fails if the response code is not expected
--
-- * fails if an error happened
rcptTo :: Connection
       -> EmailAddress
       -> SMTP Bool
rcptTo con emailAddr = do
    resp <- sendSMTPCommand con (RCPT fPath emptyESMTPParameters)
    case code resp of
        RC250Ok                               -> return True
        RC251UserNotLocalButTry               -> return True
        RC553ActionRejectedMailboxNotAllowed  -> return False
        RC550ActionRejectedMailboxUnavailable -> return False
        _ -> smtpReportError $ show (code resp) ++ "  " ++ BC.unpack (toBytes $ code resp)
  where
    fPath :: ForwardPath
    fPath = ForwardPath $ Path
        { paths = []
        , address = emailAddr
        }

-- | Send the Server the command to start sending data
startSendData :: Connection -> SMTP ()
startSendData con = do
    _ <- sendSMTPCommandWith con DATA [ RC250Ok, RC354StartMailInput ]
    return ()

-- | Send the Server the email data has been transfered and the email
-- can now be delivered.
stopSendData :: Connection -> SMTP ()
stopSendData con = do
    smtpReportInfo "terminate Email Content input streaming"
    smtpLiftIO "send DATA input terminale symboles" $ connectionPut con "\r\n.\r\n"
    _ <- recvSMTPResponse con >>= filterSMTPResponse [ RC250Ok ]
    return ()

-- | send the email content to the connected server
-- (the MIME headers are not added by this function and it is the responsability
-- of the user to create well-formated email headers)
--
-- > withSendData con action == startSendData con >> (smtpLiftIO "action" (action con)) >> stopSendData con
-- >  where
-- >    action :: Connection -> IO ()
-- >    action con = connectionPut con "Hello,\n\nThis is my first email\n\n-- \nNicolas"
withSendData :: Connection
             -> (Connection -> IO ())
             -> SMTP ()
withSendData con action = do 
    startSendData con
    smtpLiftIO "action to send data" (action con)
    stopSendData con

-- | check if a line is present in the given Response
-- This command is case insensitive
doesResponseContainsLine :: Response -> String -> Bool
doesResponseContainsLine resp line = expecLine `elem` listLines
  where
    expecLine :: String
    expecLine = map toUpper line
    listLines :: [String]
    listLines = map (\str -> map toUpper $ BC.unpack str) $ message resp

-- | this command is an helper to send, received and filter response
--
-- Equivalent to:
-- > sendSMTPCommandWith con cmd rcs == sendSMTPCommand con cmd >>= filterSMTPResponse rcs
sendSMTPCommandWith :: Connection -> Command -> [ResponseCode] -> SMTP Response
sendSMTPCommandWith con cmd rcs =
    sendSMTPCommand con cmd >>= filterSMTPResponse rcs

-- | filter a response with a list of expected response code
filterSMTPResponse :: [ResponseCode] -> Response -> SMTP Response
filterSMTPResponse codes resp
    | code resp `elem` codes = return resp
    | otherwise = smtpReportError $ show (code resp) ++ "  " ++ BC.unpack (toBytes $ code resp)

-- | This is the most basic function SMTP client offers:
--
-- it only sends the Command to the given connection, receives the response
-- and returns the response in a structured format.
--
-- reports the sent command and the received message
sendSMTPCommand :: Connection -> Command -> SMTP Response
sendSMTPCommand con cmd = do
    smtpReportCommand cmd
    smtpLiftIO "send command message" $ connectionPut con cmdBS
    recvSMTPResponse con
  where
    cmdBS :: ByteString
    cmdBS = toBytes cmd

-- | Just reads from the given connection, parses the response
-- and returns the response in a structured format.
--
-- also report the received message
recvSMTPResponse :: Connection -> SMTP Response
recvSMTPResponse con = do
    respBS <- smtpLiftIO "read repsonse message" $ connectionGet con 2048
    case parseResponseByteString respBS of
        Left  err  -> smtpReportError err
        Right resp -> do
            smtpReportResponse resp
            return resp
