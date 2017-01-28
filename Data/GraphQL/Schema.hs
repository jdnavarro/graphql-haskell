{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.Schema
  ( Schema
  , Resolver
  , Subs
  , object
  , objectA
  , scalar
  , scalarA
  , array
  , arrayA
  , enum
  , enumA
  , resolvers
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

import Control.Applicative (Alternative((<|>), empty))
import Data.Bifunctor (first)
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(Alt,getAlt))

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T (unwords)

import Data.GraphQL.AST.Core
import Data.GraphQL.Error

-- | A GraphQL schema.
--   @f@ is usually expected to be an instance of 'Alternative'.
type Schema f = NonEmpty (Resolver f)

-- | Resolves a 'Field' into an @Aeson.@'Aeson.Object' with error information
--   (or 'empty'). @f@ is usually expected to be an instance of 'Alternative'.
type Resolver f = Field -> CollectErrsT f Aeson.Object

-- | Variable substitution function.
type Subs = Text -> Maybe Text

object :: Alternative f => Name -> [Resolver f] -> Resolver f
object name resolvs = objectA name $ \case
      [] -> resolvs
      _  -> empty

-- | Like 'object' but also taking 'Argument's.
objectA
  :: Alternative f
  => Name -> ([Argument] -> [Resolver f]) -> Resolver f
objectA name f fld@(Field _ _ args sels) = withField name (resolvers (f args) sels) fld

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (Alternative f, Aeson.ToJSON a) => Text -> a -> Resolver f
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> empty

-- | Like 'scalar' but also taking 'Argument's.
scalarA
  :: (Alternative f, Aeson.ToJSON a)
  => Name -> ([Argument] -> f a) -> Resolver f
scalarA name f fld@(Field _ _ args []) = withField name (errWrap $ f args) fld
scalarA _ _ _ = empty

array :: Alternative f => Text -> [[Resolver f]] -> Resolver f
array name resolvs = arrayA name $ \case
    [] -> resolvs
    _  -> empty

-- | Like 'array' but also taking 'Argument's.
arrayA
  :: Alternative f
  => Text -> ([Argument] -> [[Resolver f]]) -> Resolver f
arrayA name f fld@(Field _ _ args sels) =
     withField name (joinErrs $ traverse (`resolvers` sels) $ f args) fld

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: Alternative f => Text -> f [Text] -> Resolver f
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

-- | Like 'enum' but also taking 'Argument's.
enumA :: Alternative f => Text -> ([Argument] -> f [Text]) -> Resolver f
enumA name f fld@(Field _ _ args []) = withField name (errWrap $ f args) fld
enumA _ _ _ = empty

-- | Helper function to facilitate 'Argument' handling.
withField
  :: (Alternative f, Aeson.ToJSON a)
  => Name -> CollectErrsT f a -> Field -> CollectErrsT f (HashMap Text Aeson.Value)
withField name f (Field alias name' _ _) =
     if name == name'
        then fmap (first $ HashMap.singleton aliasOrName . Aeson.toJSON) f
        else empty
     where
       aliasOrName = fromMaybe name' alias

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolvers :: Alternative f => [Resolver f] -> [Field] -> CollectErrsT f Aeson.Value
resolvers resolvs =
    fmap (first Aeson.toJSON . fold)
  . traverse (\fld -> getAlt (foldMap (Alt . ($ fld)) resolvs) <|> errmsg fld)
  where
    errmsg (Field alias name _ _) = addErrMsg msg $ (errWrap . pure) val
       where
         val = HashMap.singleton aliasOrName Aeson.Null
         msg = T.unwords ["field", name, "not resolved."]
         aliasOrName = fromMaybe name alias
