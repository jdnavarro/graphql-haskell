{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
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
  , withField
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Control.Arrow (first)
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Data.Monoid (Monoid(mempty,mappend))
#else
import Data.Bifunctor (first)
import Data.Monoid (Alt(Alt,getAlt))
#endif
import Control.Applicative (Alternative((<|>), empty))
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T (null, unwords)

import Data.GraphQL.AST
import Data.GraphQL.Error

-- | A GraphQL schema.
--   @f@ is usually expected to be an instance of 'Alternative'.
data Schema f = Schema [Resolver f]

-- | Resolves a 'Field' into an @Aeson.@'Aeson.Object' with error information
--   (or 'empty'). @f@ is usually expected to be an instance of 'Alternative'.
type Resolver f = Field -> CollectErrsT f Aeson.Object

-- | Variable substitution function.
type Subs = Text -> Maybe Text

-- | Create a named 'Resolver' from a list of 'Resolver's.
object :: Alternative f => Text -> [Resolver f] -> Resolver f
object name resolvs = objectA name $ \case
     [] -> resolvs
     _  -> empty

-- | Like 'object' but also taking 'Argument's.
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

-- | Like 'scalar' but also taking 'Argument's.
scalarA
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> ([Argument] -> f a) -> Resolver f
scalarA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
scalarA _ _ _ = empty

-- | Like 'object' but taking lists of 'Resolver's instead of a single list.
array :: Alternative f => Text -> [[Resolver f]] -> Resolver f
array name resolvs = arrayA name $ \case
    [] -> resolvs
    _  -> empty

-- | Like 'array' but also taking 'Argument's.
arrayA
  :: Alternative f
  => Text -> ([Argument] -> [[Resolver f]]) -> Resolver f
arrayA name f fld@(Field _ _ args _ sels) =
     withField name (joinErrs $ traverse (flip resolvers $ fields sels) $ f args) fld

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: Alternative f => Text -> f [Text] -> Resolver f
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

-- | Like 'enum' but also taking 'Argument's.
enumA :: Alternative f => Text -> ([Argument] -> f [Text]) -> Resolver f
enumA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
enumA _ _ _ = empty

-- | Helper function to facilitate 'Argument' handling.
withField
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> CollectErrsT f a -> Field -> CollectErrsT f (HashMap Text Aeson.Value)
withField name f (Field alias name' _ _ _) =
     if name == name'
        then fmap (first $ HashMap.singleton aliasOrName . Aeson.toJSON) f
        else empty
     where
       aliasOrName = if T.null alias then name' else alias

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolvers :: Alternative f => [Resolver f] -> [Field] -> CollectErrsT f Aeson.Value
resolvers resolvs =
    fmap (first Aeson.toJSON . fold)
  . traverse (\fld -> getAlt (foldMap (Alt . ($ fld)) resolvs) <|> errmsg fld)
  where
    errmsg (Field alias name _ _ _) = addErrMsg msg $ (errWrap . pure) val
       where
         val = HashMap.singleton aliasOrName Aeson.Null
         msg = T.unwords ["field", name, "not resolved."]
         aliasOrName = if T.null alias then name else alias

-- | Checks whether the given 'Selection' contains a 'Field' and
--   returns the 'Field' if so, else returns 'Nothing'.
field :: Selection -> Maybe Field
field (SelectionField x) = Just x
field _ = Nothing

-- | Returns a list of the 'Field's contained in the given 'SelectionSet'.
fields :: SelectionSet -> [Field]
fields = catMaybes . fmap field

#if !MIN_VERSION_base(4,8,0)
newtype Alt f a = Alt {getAlt :: f a}

instance Alternative f => Monoid (Alt f a) where
        mempty = Alt empty
        Alt x `mappend` Alt y = Alt $ x <|> y
#endif
