{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), pure)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, empty)
import Data.Foldable (fold)

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap

import Data.GraphQL.AST
import Data.GraphQL.Schema

execute :: (Alternative f, Monad f) => Schema f -> Document -> f Aeson.Value
execute (Schema resolv) doc = selectionSet resolv =<< query doc

query :: Applicative f => Document -> f SelectionSet
query (Document [DefinitionOperation (Query (Node _ _ _ sels))]) = pure sels
query _ = error "query: Not implemented yet"

selectionSet :: (Alternative f, Monad f) => Resolver f -> SelectionSet -> f Aeson.Value
selectionSet resolv sels = Aeson.Object . fold <$> traverse (selection resolv) sels

selection :: (Alternative f, Monad f) => Resolver f -> Selection -> f Aeson.Object
selection resolv (SelectionField (Field _ n _ _ sfs)) =
    fmap (HashMap.singleton n) $ output sfs =<< resolv (InputField n)
selection _ _ = error "selection: Not implemented yet"

output :: (Alternative f, Monad f) => SelectionSet -> Output f -> f Aeson.Value
output sels (OutputResolver resolv) = selectionSet resolv sels
output sels (OutputList os) = fmap array . traverse (output sels) =<< os
output sels (OutputEnum e)
   | null sels = Aeson.toJSON <$> e
   | otherwise = empty
output sels (OutputScalar s)
   | null sels = Aeson.toJSON <$> s
   | otherwise = empty

array :: [Aeson.Value] -> Aeson.Value
array = Aeson.toJSON
