{-# LANGUAGE CPP #-}
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

execute
  :: Alternative f
  => Schema.Schema f -> Schema.Subs -> Document -> f Aeson.Value
execute (Schema resolvs) subs = Schema.resolvers resolvs . rootFields subs

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
    Argument n . ValueString . StringValue <$> subs v
subsArg _ arg = Just arg
