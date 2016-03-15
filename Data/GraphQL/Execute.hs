{-# LANGUAGE CPP #-}
-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
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

-- | Takes a 'Schema', a variable substitution function ('Schema.Subs'), and a
--   @GraphQL@ 'document'. The substitution is applied to the document using
--  'rootFields', and the 'Schema''s resolvers are applied to the resulting fields.
--
--   Returns the result of the query against the 'Schema' wrapped in a /data/ field, or
--   errors wrapped in an /errors/ field.
execute :: Alternative f
  => Schema.Schema f -> Schema.Subs -> Document -> f Aeson.Value
execute (Schema resolvs) subs doc = runCollectErrs res
  where res = Schema.resolvers resolvs $ rootFields subs doc

-- | Takes a variable substitution function and a @GraphQL@ document.
--   If the document contains one query (and no other definitions)
--   it applies the substitution to the query's set of selections
--   and then returns their fields.
rootFields :: Schema.Subs -> Document -> [Field]
rootFields subs (Document [DefinitionOperation (Query (Node _varDefs _ _ sels))]) =
    Schema.fields $ substitute subs <$> sels
rootFields _ _ = []

-- | Takes a variable substitution function and a selection. If the
--   selection is a field it applies the substitution to the field's
--   arguments using 'subsArg', and recursively applies the substitution to
--   the arguments of fields nested in the primary field.
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
-- | Takes a variable substitution function and an argument. If the
--   argument's value is a variable the substitution is applied to the
--   variable's name.
subsArg :: Schema.Subs -> Argument -> Maybe Argument
subsArg subs (Argument n (ValueVariable (Variable v))) =
    Argument n . ValueString <$> subs v
subsArg _ arg = Just arg
