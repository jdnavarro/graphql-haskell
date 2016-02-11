{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, empty)
import Data.Foldable (fold)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import Data.GraphQL.AST
import Data.GraphQL.Schema (Resolver, Schema(..))
import qualified Data.GraphQL.Schema as Schema

execute :: (Alternative m, Monad m) => Schema m -> Document -> m Aeson.Value
execute (Schema resolv) doc = selectionSet resolv =<< query doc

query :: Alternative f => Document -> f SelectionSet
query (Document [DefinitionOperation (Query (Node _ _ _ sels))]) = pure sels
query _ = empty

selectionSet :: Alternative f => Resolver f -> SelectionSet -> f Aeson.Value
selectionSet resolv = fmap (Aeson.Object . fold) . traverse (selection resolv)

selection :: Alternative f => Resolver f -> Selection -> f Aeson.Object
selection resolv (SelectionField field@(Field _ name _ _ _)) =
    fmap (HashMap.singleton name) $ Aeson.toJSON <$> resolv (fieldToInput field)
selection _ _ = empty

-- * AST/Schema conversions

argument :: Argument -> Schema.Argument
argument (Argument n (ValueInt v)) = (n, Schema.ScalarInt $ fromIntegral v)
argument (Argument n (ValueString (StringValue v))) = (n, Schema.ScalarString v)
argument _ = error "argument: not implemented yet"

fieldToInput :: Field -> Schema.Input
fieldToInput (Field _ n as _ sels) =
  Schema.InputField n (argument <$> as) (fieldToInput . selectionToField <$> sels)

selectionToField :: Selection -> Field
selectionToField (SelectionField x) = x
selectionToField _ = error "selectionField: not implemented yet"
