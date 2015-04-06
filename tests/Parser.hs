-- |
-- Module      :
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

module Parser
    ( test_parsing_objects
    ) where

import Data.Byteable
import Data.ByteString (ByteString)
import Data.ByteString.Parse (Parser, parse, Result(..))
import qualified Data.ByteString.Char8 as BC
import Control.Applicative
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

import Network.SMTP.Parser
import Network.SMTP.Types
import Arbitrary

-- TODO
-- BSPARSE: make a method to not have to append the white space at the end of the
-- bytestring to stop the parser properly
test_parse :: Parser a -> ByteString -> a
test_parse parser bs = case parse parser (BC.snoc bs '\0') of
    ParseFail err -> error $ "failed to parse: " ++ show bs ++ " " ++ err
    ParseMore f   -> error $ "failed to parse: " ++ show bs ++ " not enough bytes"
    ParseOK _ v   -> v

test_equality :: (Show value, Eq value)
              => value
              -> value
              -> Bool
test_equality v1 v2
    | v1 == v2  = True
    | otherwise = error $ "Equality test failed: \n" ++ show v1 ++ "\n /=\n" ++ show v2

property_parser_equality :: (Show value, Eq value, Byteable value, Arbitrary value)
                         => Parser value
                         -> value
                         -> Bool
property_parser_equality parser v = test_equality v v2
  where
    v1 = test_parse parser $ toBytes v
    v2 = test_parse parser $ toBytes v1

instance Arbitrary Domain where
    arbitrary = arbitraryDomainPart
instance Arbitrary EmailAddress where
    arbitrary = arbitraryEmailAddress
instance Arbitrary ForwardPath where
    arbitrary = arbitraryForwardPath
instance Arbitrary ReversePath where
    arbitrary = arbitraryReversePath
instance Arbitrary Command where
    arbitrary = arbitraryCommand
instance Arbitrary Response where
    arbitrary = arbitraryResponse

test_parsing_objects = testGroup "parser"
    [ testProperty "Domain"       (property_parser_equality parseDomainPart)
    , testProperty "EmailAddress" (property_parser_equality parseEmailAddress)
    , testProperty "FowardPath"   (property_parser_equality parseForwardPath)
    , testProperty "ReversePath"  (property_parser_equality parseReversePath)
    , testProperty "Command"      (property_parser_equality parseCommand)
    , testProperty "Response"     (property_parser_equality parseResponse)
    ]
