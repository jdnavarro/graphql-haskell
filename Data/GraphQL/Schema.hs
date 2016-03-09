{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | This module provides the type Schema,
--   representing a GraphQL schema, and functions for defining
--   a schema.
module Data.GraphQL.Schema
  ( Schema(..)
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
  , fields
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<|>))
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Data.Monoid (Monoid(mempty,mappend))
#else
import Data.Monoid (Alt(Alt,getAlt))
#endif
import Control.Applicative (Alternative(..))
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T (null, unwords)

import Control.Arrow

import Data.GraphQL.AST
import Data.GraphQL.Error

-- | Schema represents a GraphQL schema.
--   f usually has to be an instance of Alternative.
data Schema f = Schema [Resolver f]

-- | Resolver resolves a field in to a wrapped Aeson.Object with error information
--   (or empty). The wrapped f usually has to be an instance of Alternative.
type Resolver f = Field -> CollectErrsT f Aeson.Object

-- | Subs represents a substitution.
type Subs = Text -> Maybe Text

-- | Objects represent a list of named fields, each of which
--   yield a value of a specific type.
object :: Alternative f => Text -> [Resolver f] -> Resolver f
object name resolvs = objectA name $ \case
     [] -> resolvs
     _  -> empty

-- | Fields can accept arguments to further specify the return value.
objectA
  :: Alternative f
  => Text -> ([Argument] -> [Resolver f]) -> Resolver f
objectA name f fld@(Field _ _ args _ sels) =
    withField name (resolvers (f args) $ fields sels) fld

-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (Alternative f, Aeson.ToJSON a) => Text -> a -> Resolver f
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> empty

-- | Arguments can be used to further specify a scalar's return value.
scalarA
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> ([Argument] -> f a) -> Resolver f
scalarA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
scalarA _ _ _ = empty

-- | Arrays are like objects but have an array of resolvers instead of a list.
array :: Alternative f => Text -> [[Resolver f]] -> Resolver f
array name resolvs = arrayA name $ \case
    [] -> resolvs
    _  -> empty

-- | Arguments can be used to further specify an array's return values.
arrayA
  :: Alternative f
  => Text -> ([Argument] -> [[Resolver f]]) -> Resolver f
arrayA name f fld@(Field _ _ args _ sels) =
     withField name (joinErrs $ traverse (flip resolvers $ fields sels) $ f args) fld

-- | An enum represents one of a finite set of possible values.
--   Used in place of a scalar when the possible responses are easily enumerable.
enum :: Alternative f => Text -> f [Text] -> Resolver f
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

-- | Arguments can be used to further specify an enum's return values.
enumA :: Alternative f => Text -> ([Argument] -> f [Text]) -> Resolver f
enumA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
enumA _ _ _ = empty

-- | Used to implement a resolver with arguments.
withField
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> CollectErrsT f a -> Field -> CollectErrsT f (HashMap Text Aeson.Value)
withField name f (Field alias name' _ _ _) =
     if name == name'
        then fmap (first $ HashMap.singleton aliasOrName . Aeson.toJSON) f
        else empty
     where
       aliasOrName = if T.null alias then name' else alias

-- | resolvers takes a list of resolvers and a list of fields,
--   and applies each resolver to each field. Resolves into a value
--   containing the resolved field, or a null value and error information.
resolvers :: Alternative f => [Resolver f] -> [Field] -> CollectErrsT f Aeson.Value
resolvers resolvs =
    fmap (first Aeson.toJSON . fold)
  . traverse (\fld -> (getAlt $ foldMap (Alt . ($ fld)) resolvs) <|> errmsg fld)
    where errmsg (Field alias name _ _ _) = addErrMsg msg $ (errWrap . pure) val
            where val = HashMap.singleton aliasOrName Aeson.Null
                  msg = T.unwords ["field", name, "not resolved."]
                  aliasOrName = if T.null alias then name else alias

-- | Checks whether the given selection contains a field and
--   returns the field if so, else returns Nothing.
field :: Selection -> Maybe Field
field (SelectionField x) = Just x
field _ = Nothing

-- | Returns a list of the fields contained in the given selection set.
fields :: SelectionSet -> [Field]
fields = catMaybes . fmap field

#if !MIN_VERSION_base(4,8,0)
newtype Alt f a = Alt {getAlt :: f a}

instance Alternative f => Monoid (Alt f a) where
        mempty = Alt empty
        Alt x `mappend` Alt y = Alt $ x <|> y
#endif
