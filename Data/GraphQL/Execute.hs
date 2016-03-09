{-# LANGUAGE CPP #-}
-- | The module Data.GraphQL.Execute provides
--   a the function execute which executes a GraphQL
--   query according to a given GraphQL schema.
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

-- | execute takes a schema, a substitution and a GraphQL document.
--   The substition is applied to the document using rootFields, and
--   the schema's resolvers are applied to the resulting fields.
--   Returns the list of the first non-empty resolutions
--   for each field (wrapped in an Aeson.Value).
execute
  :: Alternative f
  => Schema.Schema f -> Schema.Subs -> Document -> f Aeson.Value
execute (Schema resolvs) subs = Schema.resolvers resolvs . rootFields subs

-- | rootFields takes in a substitution and a GraphQL document.
--   If the document contains one query (and no other definitions)
--   it applies the substitution to the query's set of selections
--   and then returns their fields.
rootFields :: Schema.Subs -> Document -> [Field]
rootFields subs (Document [DefinitionOperation (Query (Node _varDefs _ _ sels))]) =
    Schema.fields $ substitute subs <$> sels
rootFields _ _ = []

-- | substitute takes in a substitution and a selection.
--   If the selection is a field it applies the substitution to the
--   field's arguments using subsArg,
--   and recursively applies the substitution to the arguments of fields
--   nested in the primary field.
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
-- | subsArg takes in a substitution and an argument.
--   If the argument's value is a variable the substitution
--   is applied to the variable's name.
subsArg :: Schema.Subs -> Argument -> Maybe Argument
subsArg subs (Argument n (ValueVariable (Variable v))) =
    Argument n . ValueString <$> subs v
subsArg _ arg = Just arg
