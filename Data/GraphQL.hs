module Data.GraphQL where

import Control.Applicative (Alternative, empty)

import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec

import Data.GraphQL.Execute
import Data.GraphQL.Parser
import Data.GraphQL.Schema

graphql :: (Alternative m, Monad m) => Schema m -> Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing

graphqlSubs :: (Alternative m, Monad m) => Schema m -> Subs -> Text -> m Aeson.Value
graphqlSubs schema f =
    either (const empty) (execute schema f)
  . Attoparsec.parseOnly document
