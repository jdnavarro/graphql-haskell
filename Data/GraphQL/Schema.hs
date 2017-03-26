{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.Schema
  ( Schema
  , Resolver
  , Subs
  , object
  -- , object'
  , objectA
  -- , objectA'
  , scalar
  , scalarA
  , array
  -- , array'
  , arrayA
  -- , arrayA'
  , enum
  , enumA
  , resolve
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup((<>)))

import Control.Exception.Safe.Checked (MonadCatch, Throws, catch, throw)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Data.GraphQL.Error
import Data.GraphQL.AST.Core

-- | A GraphQL schema.
--   @f@ is usually expected to be an instance of 'Alternative'.
type Schema m = NonEmpty (Resolver m)

-- | Resolves a 'Field' into an @Aeson.@'Aeson.Object' with error information
--   (or 'empty'). @f@ is usually expected to be an instance of 'Alternative'.
type Resolver m = Field -> m Aeson.Object

type Resolvers m = [Resolver m]

type Fields = [Field]

type Arguments = [Argument]

-- | Variable substitution function.
type Subs = Name -> Maybe Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: (MonadCatch m, Throws NotFound) => Name -> Resolvers m -> Resolver m
object name resolvers = objectA name $ \case
  [] -> resolvers
  _  -> throw NotFound

-- | Like 'object' but also taking 'Argument's.
objectA
  :: (MonadCatch m, Throws NotFound)
  => Name -> (Arguments -> Resolvers m) -> Resolver m
objectA name f fld@(Field _ _ args flds) =
  withField name (resolve (f args) flds) fld


-- -- | Create a named 'Resolver' from a list of 'Resolver's.
-- object' :: MonadCatch m => Text -> m [Resolver m] -> Resolver m
-- object' name resolvs = objectA' name $ \case
--      [] -> resolvs
--      _  -> unimplemented

-- -- | Like 'object'' but also taking 'Argument's.
-- objectA'
--   :: MonadCatch m
--   => Text -> ([Argument] -> m [Resolver m]) -> Resolver m
-- objectA' name f fld@(Field _ _ args flds) = do
--     resolvs <- f args
--     withField name (resolve resolvs flds) fld


-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (MonadCatch m, Throws NotFound, Aeson.ToJSON a) => Name -> a -> Resolver m
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> throw NotFound

-- | Like 'scalar' but also taking 'Argument's.
scalarA
  :: (MonadCatch m, Throws NotFound, Aeson.ToJSON a)
  => Name -> (Arguments -> m a) -> Resolver m
scalarA name f fld@(Field _ _ args []) = withField name (f args) fld
scalarA _ _ _ = throw NotFound

array :: (MonadCatch m, Throws NotFound) => Name -> [Resolvers m] -> Resolver m
array name resolvers = arrayA name $ \case
    [] -> resolvers
    _  -> throw NotFound

-- | Like 'array' but also taking 'Argument's.
arrayA
  :: (MonadCatch m, Throws NotFound)
  => Text -> (Arguments -> [Resolvers m]) -> Resolver m
arrayA name f fld@(Field _ _ args sels) =
     withField name (traverse (`resolve` sels) $ f args) fld

-- | Like 'object'' but taking lists of 'Resolver's instead of a single list.
-- array' :: MonadCatch m => Text -> m [[Resolver m]] -> Resolver m
-- array' name resolvs = arrayA' name $ \case
--     [] -> resolvs
--     _  -> unimplemented

-- | Like 'array'' but also taking 'Argument's.
-- arrayA'
--   :: MonadCatch m
--   => Text -> ([Argument] -> m [[Resolver m]]) -> Resolver m
-- arrayA' name f fld@(Field _ _ args sels) = do
--      resolvs <- f args
--      withField name (traverse (`resolve` sels) resolvs) fld

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: (MonadCatch m, Throws NotFound) => Text -> m [Text] -> Resolver m
enum name enums = enumA name $ \case
     [] -> enums
     _  -> throw NotFound

-- | Like 'enum' but also taking 'Argument's.
enumA :: (MonadCatch m, Throws NotFound) => Text -> ([Argument] -> m [Text]) -> Resolver m
enumA name f fld@(Field _ _ args []) = withField name (f args) fld
enumA _ _ _ = throw NotFound

-- | Helper function to facilitate 'Argument' handling.
withField
  :: (MonadCatch m, Throws NotFound, Aeson.ToJSON a)
  => Name -> m a -> Field -> m (HashMap Text Aeson.Value)
withField name v (Field alias name' _ _) =
  if name == name'
    then fmap (HashMap.singleton aliasOrName . Aeson.toJSON) v
         -- TODO: Report error when Non-Nullable type for field argument.
     `catch` (\(_ :: NotFound) -> pure (HashMap.singleton aliasOrName Aeson.Null))
    else throw NotFound
  where
    aliasOrName = fromMaybe name alias

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: (MonadCatch m, Throws NotFound) => Resolvers m -> Fields -> m Aeson.Value
resolve resolvers =
    fmap (Aeson.toJSON . fold)
  . traverse (\fld -> unAltNotFound (foldMap (AltNotFound . ($ fld)) resolvers))

newtype AltNotFound f a = AltNotFound { unAltNotFound :: f a }

instance (MonadCatch m, Throws NotFound) => Semigroup (AltNotFound m a) where
  AltNotFound r1 <> AltNotFound r2 = AltNotFound . catch r1 $ \(_ :: NotFound) -> r2

instance (MonadCatch m, Throws NotFound) => Monoid (AltNotFound m a) where
  mempty = AltNotFound $ throw NotFound
  mappend = (<>)
