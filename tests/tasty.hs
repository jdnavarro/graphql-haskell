{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as Text
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit

import qualified Data.GraphQL.Parser as Parser
import qualified Data.GraphQL.Encoder as Encoder

import Paths_graphql (getDataFileName)

main :: IO ()
main = defaultMain =<< testCase "Kitchen Sink"
                   <$> (assertEqual "Encode" <$> expected <*> actual)
  where
    expected = Text.readFile
           =<< getDataFileName "tests/data/kitchen-sink.min.graphql"

    actual = either (error "Parsing error!") Encoder.document
         <$> parseOnly Parser.document
         <$> expected
