-- |
-- Module      : Main
-- License     : BSD-Style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

module Main (main) where

import Test.Tasty
import Parser

main :: IO ()
main = defaultMain $ testGroup "smtp"
    [ test_parsing_objects
    ]
