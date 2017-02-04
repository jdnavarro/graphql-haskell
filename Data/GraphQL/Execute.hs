-- | This module provides the function to execute a @GraphQL@ request --
--   according to a 'Schema'.
module Data.GraphQL.Execute (execute) where

import Control.Applicative (Alternative, empty)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Data.Aeson as Aeson

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
document schema (op :| [])= operation schema op
document _ _ = error "Multiple operations not supported yet"

operation :: Alternative f => Schema f -> AST.Core.Operation -> f Aeson.Value
operation schema (AST.Core.Query flds) =
  Schema.resolve (NE.toList schema) (NE.toList flds)
operation _ _ = error "Mutations not supported yet"

-- | Takes a variable substitution function and a @GraphQL@ document.
--   If the document contains one query (and no other definitions)
--   it applies the substitution to the query's set of selections
--   and then returns their fields.
-- rootFields :: Schema.Subs -> Document -> [Field]
-- rootFields subs (Document [DefinitionOperation (Query (Node _varDefs _ _ sels))]) =
--     Schema.fields $ substitute subs <$> sels
-- rootFields _ _ = []

-- | Takes a variable substitution function and a selection. If the
--   selection is a field it applies the substitution to the field's
--   arguments using 'subsArg', and recursively applies the substitution to
--   the arguments of fields nested in the primary field.
-- substitute :: Schema.Subs -> Selection -> Selection
-- substitute subs (SelectionField (Field alias name args directives sels)) =
--     SelectionField $ Field
--       alias
--       name
--       -- TODO: Get rid of `catMaybes`, invalid arguments should raise an error
--       (catMaybes $ subsArg subs <$> args)
--       directives
--       (substitute subs <$> sels)
-- substitute _ sel = sel

-- TODO: Support different value types
-- | Takes a variable substitution function and an argument. If the
--   argument's value is a variable the substitution is applied to the
--   variable's name.
-- subsArg :: Schema.Subs -> Argument -> Maybe Argument
-- subsArg subs (Argument n (ValueVariable (Variable v))) =
--     Argument n . ValueString <$> subs v
-- subsArg _ arg = Just arg
