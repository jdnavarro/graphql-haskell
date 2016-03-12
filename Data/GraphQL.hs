module Data.GraphQL where

import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec

import Data.GraphQL.Execute
import Data.GraphQL.Parser
import Data.GraphQL.Schema

import Data.GraphQL.Error
import Control.Monad (MonadPlus)

graphql :: (MonadPlus m, Monad m) => Schema m -> Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing


graphqlSubs :: (MonadPlus m, Monad m) => Schema m -> Subs -> Text -> m Aeson.Value
graphqlSubs schema f =
    either parseError (execute schema f)
  . Attoparsec.parseOnly document
