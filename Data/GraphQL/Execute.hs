{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Execute (execute) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative (Alternative)
import Data.Maybe (catMaybes)

import qualified Data.Aeson as Aeson

import Data.GraphQL.AST
import Data.GraphQL.Schema (Schema(..))
import qualified Data.GraphQL.Schema as Schema

import Data.GraphQL.Error

{- | Takes a schema, a substitution and a GraphQL document.
     The substition is applied to the document using rootFields, and
     the schema's resolvers are applied to the resulting fields.
     Returns the result of the query against the schema wrapped in a
     "data" field, or errors wrapped in a "errors field".
-}
execute :: Alternative m
  => Schema.Schema m -> Schema.Subs -> Document -> m Aeson.Value
execute (Schema resolvs) subs doc = runCollectErrs res
  where res = Schema.resolvers resolvs $ rootFields subs doc


rootFields :: Schema.Subs -> Document -> [Field]
rootFields subs (Document [DefinitionOperation (Query (Node _varDefs _ _ sels))]) =
    Schema.fields $ substitute subs <$> sels
rootFields _ _ = []

substitute :: Schema.Subs -> Selection -> Selection
substitute subs (SelectionField (Field alias name args directives sels)) =
    SelectionField $ Field
      alias
      name
      -- TODO: Get rid of `catMaybes`, invalid arguments should raise an error
      (catMaybes $ subsArg subs <$> args)
      directives
      (substitute subs <$> sels)
substitute _ sel = sel

-- TODO: Support different value types
subsArg :: Schema.Subs -> Argument -> Maybe Argument
subsArg subs (Argument n (ValueVariable (Variable v))) =
    Argument n . ValueString <$> subs v
subsArg _ arg = Just arg
