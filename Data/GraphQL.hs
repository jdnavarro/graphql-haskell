-- | This module provides the functions to parse and execute @GraphQL@ queries.
module Data.GraphQL where

import Control.Applicative (Alternative)

import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec

import Data.GraphQL.Execute
import Data.GraphQL.Parser
import Data.GraphQL.Schema

import Data.GraphQL.Error

-- | Takes a 'Schema' and text representing a @GraphQL@ request document.
--   If the text parses correctly as a @GraphQL@ query the query is
--   executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphql :: Alternative m => Schema m -> Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing

-- | Takes a 'Schema', a variable substitution function and text
--   representing a @GraphQL@ request document.  If the text parses
--   correctly as a @GraphQL@ query the substitution is applied to the
--   query and the query is then executed according to the given 'Schema'.
--
--   Returns the response as an @Aeson.@'Aeson.Value'.
graphqlSubs :: Alternative m => Schema m -> Subs -> Text -> m Aeson.Value
graphqlSubs schema f =
    either parseError (execute schema f)
  . Attoparsec.parseOnly document
