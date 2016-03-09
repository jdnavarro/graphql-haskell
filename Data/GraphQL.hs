-- | The module Data.GraphQl provides the
--   functions graphql and graphqlSubs to parse
--   and execute GraphQL queries.
module Data.GraphQL where

import Control.Applicative (Alternative, empty)

import Data.Text (Text)

import qualified Data.Aeson as Aeson
import qualified Data.Attoparsec.Text as Attoparsec

import Data.GraphQL.Execute
import Data.GraphQL.Parser
import Data.GraphQL.Schema

-- | graphql takes a schema and text representing a GraphQL document.
--   If the text parses correctly as a GraphQl query document
--   the query is executed according to the given schema.
--   Returns the response to the query wrapped in an Aeson.Value.
graphql :: (Alternative m, Monad m) => Schema m -> Text -> m Aeson.Value
graphql = flip graphqlSubs $ const Nothing

-- | graphqlsubs takes in a schema, a substitution and text representing
--   a GraphQL document.
--   If the text parses correctly as a GraphQl query document the
--   substitution is applied to the query and
--   the query is then executed according to the given schema.
--   Returns the response to the query wrapped in an Aeson.Value.
graphqlSubs :: (Alternative m, Monad m) => Schema m -> Subs -> Text -> m Aeson.Value
graphqlSubs schema f =
    either (const empty) (execute schema f)
  . Attoparsec.parseOnly document
