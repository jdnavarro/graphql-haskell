{-# LANGUAGE OverloadedStrings #-}
-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
module Data.GraphQL.Execute (execute) where

import Control.Applicative (Alternative, empty)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import qualified Data.GraphQL.AST as AST
import qualified Data.GraphQL.AST.Core as AST.Core
import qualified Data.GraphQL.AST.Transform as Transform
import Data.GraphQL.Schema (Schema)
import qualified Data.GraphQL.Schema as Schema

-- | Takes a 'Schema', a variable substitution function ('Schema.Subs'), and a
--   @GraphQL@ 'document'. The substitution is applied to the document using
--  'rootFields', and the 'Schema''s resolvers are applied to the resulting fields.
--
--   Returns the result of the query against the 'Schema' wrapped in a /data/ field, or
--   errors wrapped in an /errors/ field.
execute
  :: (Alternative f, Monad f)
  => Schema f -> Schema.Subs -> AST.Document -> f Aeson.Value
execute schema subs doc = document schema =<< maybe empty pure (Transform.document subs doc)

document :: Alternative f => Schema f -> AST.Core.Document -> f Aeson.Value
document schema (op :| []) = operation schema op
document _ _ = error "Multiple operations not supported yet"

operation :: Alternative f => Schema f -> AST.Core.Operation -> f Aeson.Value
operation schema (AST.Core.Query flds) =
  Aeson.Object . HashMap.singleton "data"
             <$> Schema.resolve (NE.toList schema) (NE.toList flds)
operation _ _ = error "Mutations not supported yet"
