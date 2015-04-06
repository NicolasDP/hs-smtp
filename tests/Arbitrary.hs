-- |
-- Module      : Arbitrary
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

{-# LANGUAGE OverloadedStrings #-}

module Arbitrary
    ( -- * Types
      arbitraryEmailAddress
    , arbitraryForwardPath
    , arbitraryReversePath
    , arbitraryCommand
    , arbitraryResponse

      -- * Others
    , arbitraryPath
    , arbitraryLocalPart
    , arbitraryDomainPart
    ) where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import Test.QuickCheck

import Network.SMTP.Types

arbitraryMaybe :: Gen a -> Gen (Maybe a)
arbitraryMaybe gen = frequency
    [ (9, Just <$> gen)
    , (1, return Nothing)
    ]

repeatGen :: (Int, Int) -> Gen a -> Gen [a]
repeatGen boundaries gen = do
    size <- choose boundaries
    vectorOf size gen

letterDigits :: [Char]
letterDigits = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

arbitraryLetterDigit :: Gen Char
arbitraryLetterDigit = elements letterDigits

arbitraryLdhStr :: Gen ByteString
arbitraryLdhStr = do
    l <- BC.pack <$> repeatGen (0, 61) (elements ('-' : letterDigits))
    if BC.null l || BC.last l /= '-'
        then return l
        else BC.snoc l <$> arbitraryLetterDigit

arbitraryDomainNode :: Gen ByteString
arbitraryDomainNode = frequency
    [ (10, BC.singleton <$> arbitraryLetterDigit)
    , (90, BC.cons <$> arbitraryLetterDigit <*> arbitraryLdhStr)
    ]

arbitraryDomain :: Gen Domain
arbitraryDomain = do
    nodes <- BC.intercalate "." <$> repeatGen (1, 4) arbitraryDomainNode
    root  <- BC.pack <$> repeatGen (2, 5) arbitraryLetterDigit
    return $ Domain $ BC.intercalate "." [nodes, root]

arbitraryDomainPart :: Gen Domain
arbitraryDomainPart = frequency
    [ (90, arbitraryDomain)
    -- , (10, arbitraryLiteralAddress)
    ]

arbitraryAtom :: Gen ByteString
arbitraryAtom = BC.pack <$> repeatGen (1, 42) arbitraryAtomEl
  where
    arbitraryAtomEl :: Gen Char
    arbitraryAtomEl = frequency
        [ (40, elements ['a'..'z'])
        , (30, elements ['A'..'Z'])
        , (10, elements "!#$%&'*+-/=?^_`{|}~")
        ]

arbitraryDotLocalPart :: Gen LocalPart
arbitraryDotLocalPart = LocalPart . BC.intercalate "." <$> repeatGen (1, 5) arbitraryAtom

arbitraryLocalPart :: Gen LocalPart
arbitraryLocalPart = frequency
    [ (90, arbitraryDotLocalPart)
    -- , (10, arbitraryQuotedString)
    ]

arbitraryEmailAddress :: Gen EmailAddress
arbitraryEmailAddress = EmailAddress
    <$> arbitraryLocalPart
    <*> arbitraryDomainPart

arbitraryPath :: Gen Path
arbitraryPath = Path
    <$> (repeatGen (0, 8) arbitraryDomainPart)
    <*> arbitraryEmailAddress

arbitraryMailParameter :: Gen (ESMTPKeyWord, Maybe ESMTPValue)
arbitraryMailParameter = (,)
    <$> arbitraryDomainNode
    <*> arbitraryMaybe arbitraryLdhStr

arbitraryMailParameters :: Gen ESMTPParameters
arbitraryMailParameters = Map.fromList <$>
    repeatGen (0, 10) arbitraryMailParameter

arbitraryForwardPath :: Gen ForwardPath
arbitraryForwardPath = ForwardPath <$> arbitraryPath

arbitraryReversePath :: Gen ReversePath
arbitraryReversePath = ReversePath <$> arbitraryMaybe arbitraryPath

arbitraryHELO :: Gen Command
arbitraryHELO = HELO <$> arbitraryDomainPart

arbitraryEHLO :: Gen Command
arbitraryEHLO = EHLO <$> arbitraryDomainPart

arbitraryMAIL :: Gen Command
arbitraryMAIL = MAIL <$> arbitraryReversePath <*> arbitraryMailParameters

arbitraryRCPT :: Gen Command
arbitraryRCPT = RCPT <$> arbitraryForwardPath <*> arbitraryMailParameters

arbitraryDATA :: Gen Command
arbitraryDATA = return DATA

arbitraryEXPN :: Gen Command
arbitraryEXPN = EXPN <$> arbitraryLdhStr

arbitraryVRFY :: Gen Command
arbitraryVRFY = VRFY <$> arbitraryLdhStr

arbitraryHELP :: Gen Command
arbitraryHELP = HELP <$> arbitraryMaybe arbitraryLdhStr

arbitraryAuthType :: Gen AuthType
arbitraryAuthType = frequency
    [ (1, return PLAIN)
    , (2, return LOGIN)
    , (3, return CRAM_MD5)
    ]

arbitraryAUTH :: Gen Command
arbitraryAUTH = AUTH <$> arbitraryAuthType <*> arbitraryMaybe arbitraryLdhStr

arbitraryNOOP :: Gen Command
arbitraryNOOP = NOOP <$> arbitraryMaybe arbitraryLdhStr

arbitraryRSET :: Gen Command
arbitraryRSET = return RSET

arbitraryQUIT :: Gen Command
arbitraryQUIT = return QUIT

arbitrarySTARTTLS :: Gen Command
arbitrarySTARTTLS = return STARTTLS

arbitraryCommand :: Gen Command
arbitraryCommand = oneof
    [ arbitraryHELO
    , arbitraryEHLO
    , arbitraryMAIL
    , arbitraryRCPT
    , arbitraryDATA
    , arbitraryEXPN
    , arbitraryVRFY
    , arbitraryHELP
    , arbitraryAUTH
    , arbitraryNOOP
    , arbitraryRSET
    , arbitraryQUIT
    , arbitrarySTARTTLS
    ]

arbitraryResponseCode :: Gen ResponseCode
arbitraryResponseCode = oneof
    [ return RC500SyntaxCommandError
    , return RC501SyntaxParameterError
    , return RC502CommandNotImplemented
    , return RC503BadSequenceOfCommand
    , return RC504ParameterNotImplemented
    , return RC211SystemStatus
    , return RC214Help
    , return RC220ServiceReady
    , return RC221ServiceClosingChannel
    , return RC421ServiceNotAvailable
    , return RC250Ok
    , return RC251UserNotLocalButTry
    , return RC252CannotVRFYUser
    , return RC455ServerUnableToAccParrameters
    , return RC555FromOrToError
    , return RC450MailActionRejectedMailboxUnavailable
    , return RC550ActionRejectedMailboxUnavailable
    , return RC451ActionAborted
    , return RC551UserNotLocalFail
    , return RC452ActionRejectedSystemStorage
    , return RC552MailActionAbbortedSystemStorage
    , return RC553ActionRejectedMailboxNotAllowed
    , return RC354StartMailInput
    , return RC554Failed
    , RCUnknown <$> choose (100, 200)
    ]

arbitraryResponse :: Gen Response
arbitraryResponse = Response
    <$> arbitraryResponseCode
    <*> repeatGen (0, 10) arbitraryLdhStr
