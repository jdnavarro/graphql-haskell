{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as Text
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit

import qualified Data.GraphQL.Parser as Parser
import qualified Data.GraphQL.Printer as Printer

import Paths_graphql (getDataFileName)

main :: IO ()
main = defaultMain =<< testCase "Kitchen Sink"
                   <$> (assertEqual "Encode" <$> expected <*> actual)
  where
    expected = Text.readFile
           =<< getDataFileName "tests/data/kitchen-sink.min.graphql"

    actual = either (error "Parsing error!") Printer.document
         <$> parseOnly Parser.document
         <$> expected