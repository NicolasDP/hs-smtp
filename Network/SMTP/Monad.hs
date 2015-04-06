module Network.SMTP.Monad
    ( SMTP
    , Result(..)
    , SMTPReport(..)
    , SMTPReports(..)
    , executeSMTP
    -- * Report
    , smtpReportInfo
    , smtpReportError
    , smtpReportCommand
    , smtpReportResponse
    -- * IO
    , smtpLiftIO
    ) where

import Control.Applicative
import Data.Hourglass
import Network.SMTP.Types
import System.Hourglass
import Control.Exception (SomeException, try)

-- | Report message
data SMTPReport
    = SMTPReportInfo
        { smtpReportDate   :: !ElapsedP
            -- ^ the date when the info message has been reported
        , smtpReportReport :: !String
            -- ^ the message
        }
        -- ^ informative message
    | SMTPReportError
        { smtpReportDate   :: !ElapsedP
            -- ^ the date when the error message has been reported
        , smtpReportReport :: !String
            -- ^ the reported error
        }
        -- ^ report an error
    | SMTPReportCommand
        { smtpReportDate :: !ElapsedP
            -- ^ the date when the command message has been reported
        , smtpReportCmd  :: !Command
            -- ^ the reported command
        }
    | SMTPReportResponse
        { smtpReportDate      :: !ElapsedP
            -- ^ the date when the response has been reported
        , smtpReportRespCode  :: !Response
            -- ^ the reported response
        }
  deriving (Show, Eq)

-- | a collection of SMTP report
--
-- from the latest to the oldest one
newtype SMTPReports = SMTPReports
    { smtpReports :: [SMTPReport]
    } deriving (Show, Eq)

newSMTPReports :: SMTPReports
newSMTPReports = SMTPReports
    { smtpReports = []
    }

insertSMTPReports :: SMTPReports -> SMTPReport -> SMTPReports
insertSMTPReports rs r = rs
    { smtpReports = r : smtpReports rs
    }

infoSMTPReports :: SMTPReports -> String -> IO SMTPReports
infoSMTPReports rs infoMsg = do
    t <- timeCurrentP
    return $ insertSMTPReports rs $ SMTPReportInfo t infoMsg

failSMTPReports :: SMTPReports -> String -> IO SMTPReports
failSMTPReports rs errorMsg = do
    t <- timeCurrentP
    return $ insertSMTPReports rs $ SMTPReportError t errorMsg

commandSMTPReports :: SMTPReports -> Command -> IO SMTPReports
commandSMTPReports rs cmd = do
    t <- timeCurrentP
    return $ insertSMTPReports rs $ SMTPReportCommand t cmd

responseSMTPReports :: SMTPReports
                    -> Response
                    -> IO SMTPReports
responseSMTPReports rs resp = do
    t <- timeCurrentP
    return $ insertSMTPReports rs $ SMTPReportResponse t resp

-- Result ---------------------------------------------------------------------

-- | a result
data Result a
    = SMTPOK SMTPReports a
        -- ^ no failure to report:
        -- return the Reports and the value
    | SMTPKO SMTPReports
        -- ^ something goes wrong: returns the Reports
  deriving (Show, Eq)

-- Monad ----------------------------------------------------------------------

-- | the SMTP Monad
--
-- just a simplement Monad to report action, state, command and response
--
-- > fail "error" == smtpReportError "error"
newtype SMTP a = SMTP
    { runSMTP :: SMTPReports -> IO (Result a)
    }

instance Functor SMTP where
    fmap = fmapSMTP

instance Applicative SMTP where
    pure  = returnSMTP
    (<*>) = appendSMTP

instance Monad SMTP where
    return = returnSMTP
    (>>=)  = bindSMTP
    fail   = smtpReportError

fmapSMTP :: (a -> b) -> SMTP a -> SMTP b
fmapSMTP f m = SMTP $ \acc -> do
    r <- runSMTP m acc
    return $ case r of
        SMTPOK acc' v -> SMTPOK acc' (f v)
        SMTPKO acc'   -> SMTPKO acc'

returnSMTP :: value -> SMTP value
returnSMTP value = SMTP $ \acc -> return $ SMTPOK acc value

appendSMTP :: SMTP (a -> b) -> SMTP a -> SMTP b
appendSMTP m1f m2 = m1f >>= \f -> m2 >>= \v -> return (f v)

bindSMTP :: SMTP a -> (a -> SMTP b) -> SMTP b
bindSMTP m f = SMTP $ \acc -> do
    r <- runSMTP m acc
    case r of
        SMTPOK acc' v -> runSMTP (f v) acc'
        SMTPKO acc'   -> return $ SMTPKO acc'

-- | a little helper to execute the given SMTP
executeSMTP :: SMTP a -> IO (Result a)
executeSMTP m = runSMTP m newSMTPReports

-- Monad report ---------------------------------------------------------------

-- | insert an informative message into the report
smtpReportInfo :: String -> SMTP ()
smtpReportInfo infoMsg = SMTP $ \acc ->
    (flip SMTPOK () <$> infoSMTPReports acc infoMsg)

-- | report an error and fails
-- 
-- > smtpReportError "something goes wrong" == fail "something goes wrong"
smtpReportError :: String -> SMTP a
smtpReportError errorMsg = SMTP $ \acc ->
    (SMTPKO <$> failSMTPReports acc errorMsg)

-- | report a command (to use before sending a command)
smtpReportCommand :: Command -> SMTP ()
smtpReportCommand cmd = SMTP $ \acc ->
    (flip SMTPOK () <$> commandSMTPReports acc cmd)

-- | report a response (to use before receiving a response)
smtpReportResponse :: Response -> SMTP ()
smtpReportResponse resp = SMTP $ \acc ->
    (flip SMTPOK () <$> responseSMTPReports acc resp)

-- Monad IO report ------------------------------------------------------------

catchAll :: IO a -> IO (Either SomeException a)
catchAll = try

-- | to use to catch all kind of Exception and fail properly
smtpLiftIO :: String
           -> IO response
           -> SMTP response
smtpLiftIO infoMsg action = SMTP $ \acc -> do
    response <- catchAll action
    case response of
        Right value  -> return $ SMTPOK acc value
        Left  errMsg -> SMTPKO <$> failSMTPReports acc (infoMsg ++ ": " ++ show errMsg)
