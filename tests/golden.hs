{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif
import Control.Monad ((>=>))
import Data.Attoparsec.Text (parseOnly)
import Data.ByteString.Lazy.Char8 as B8
import qualified Data.Text.IO as TIO
import Test.Tasty (defaultMain)
import Test.Tasty.Golden (goldenVsString)

import Paths_graphql (getDataFileName)
import Data.GraphQL.Parser (document)

main :: IO ()
main = defaultMain
   =<< goldenVsString "kitchen-sink.graphql"
   <$> getDataFileName "tests/data/kitchen-sink.graphql.graphql.golden"
   <*> (parse <$> getDataFileName "tests/data/kitchen-sink.graphql")
  where
    parse = fmap (parseOnly document) . TIO.readFile
        >=> pure . either B8.pack (flip B8.snoc '\n' . B8.pack . show)
