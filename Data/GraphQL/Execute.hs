{-# LANGUAGE CPP #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, empty)
import Data.Foldable (fold)
import Data.Maybe (catMaybes)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import Data.GraphQL.AST
import Data.GraphQL.Schema (Resolver, Schema(..))
import qualified Data.GraphQL.Schema as Schema

execute :: (Alternative m, Monad m) => Schema m -> Schema.Subs -> Document -> m Aeson.Value
execute (Schema resolv) f doc = selectionSet f resolv =<< query doc

query :: Alternative f => Document -> f SelectionSet
query (Document [DefinitionOperation (Query (Node _varDefs _ _ sels))]) =
  pure sels
query  _ = empty

selectionSet :: Alternative f => Schema.Subs -> Resolver f -> SelectionSet -> f Aeson.Value
selectionSet f resolv = fmap (Aeson.Object . fold) . traverse (selection f resolv)

selection :: Alternative f => Schema.Subs -> Resolver f -> Selection -> f Aeson.Object
selection f resolv (SelectionField field@(Field _ name _ _ _)) =
    fmap (HashMap.singleton name) $ Aeson.toJSON <$> resolv (fieldToInput f field)
selection _ _ _ = empty

-- * AST/Schema conversions

argument :: Schema.Subs -> Argument -> Maybe Schema.Argument
argument f (Argument n (ValueVariable (Variable v))) =
    maybe Nothing (\v' -> Just (n, v')) $ f v
argument _ (Argument n (ValueInt v)) =
    Just (n, Schema.ScalarInt $ fromIntegral v)
argument _ (Argument n (ValueString (StringValue v))) =
    Just (n, Schema.ScalarString v)
argument _ _ = error "argument: not implemented yet"

fieldToInput :: Schema.Subs -> Field -> Schema.Input
fieldToInput f (Field _ n as _ sels) =
    Schema.InputField n (catMaybes $ argument f <$> as)
                        (fieldToInput f . selectionToField <$> sels)

selectionToField :: Selection -> Field
selectionToField (SelectionField x) = x
selectionToField _ = error "selectionField: not implemented yet"
