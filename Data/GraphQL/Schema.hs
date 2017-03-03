{-# LANGUAGE LambdaCase #-}
-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.Schema
  ( Schema
  , Resolver
  , Subs
  , object
  , object'
  , objectA
  , objectA'
  , scalar
  , scalarA
  , array
  , array'
  , arrayA
  , arrayA'
  , enum
  , enumA
  , resolve
  -- * AST Reexports
  , Field
  , Argument(..)
  , Value(..)
  ) where

import Control.Applicative (Alternative(empty), (<|>))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt(Alt,getAlt))

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Data.GraphQL.AST.Core

-- | A GraphQL schema.
--   @f@ is usually expected to be an instance of 'Alternative'.
type Schema f = NonEmpty (Resolver f)

-- | Resolves a 'Field' into an @Aeson.@'Aeson.Object' with error information
--   (or 'empty'). @f@ is usually expected to be an instance of 'Alternative'.
type Resolver f = Field -> f Aeson.Object

type Resolvers f = [Resolver f]

type Fields = [Field]

type Arguments = [Argument]

-- | Variable substitution function.
type Subs = Name -> Maybe Value

-- | Create a new 'Resolver' with the given 'Name' from the given 'Resolver's.
object :: Alternative f => Name -> Resolvers f -> Resolver f
object name resolvers = objectA name $ \case
  [] -> resolvers
  _  -> empty

-- | Like 'object' but also taking 'Argument's.
objectA
  :: Alternative f
  => Name -> (Arguments -> Resolvers f) -> Resolver f
objectA name f fld@(Field _ _ args flds) = withField name (resolve (f args) flds) fld


-- | Create a named 'Resolver' from a list of 'Resolver's.
object' :: (Alternative f, Monad f) => Text -> f [Resolver f] -> Resolver f
object' name resolvs = objectA' name $ \case
     [] -> resolvs
     _  -> empty

-- | Like 'object'' but also taking 'Argument's.
objectA'
  :: (Alternative f, Monad f)
  => Text -> ([Argument] -> f [Resolver f]) -> Resolver f
objectA' name f fld@(Field _ _ args flds) = do
    resolvs <- f args
    withField name (resolve resolvs flds) fld


-- | A scalar represents a primitive value, like a string or an integer.
scalar :: (Alternative f, Aeson.ToJSON a) => Name -> a -> Resolver f
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> empty

-- | Like 'scalar' but also taking 'Argument's.
scalarA
  :: (Alternative f, Aeson.ToJSON a)
  => Name -> (Arguments -> f a) -> Resolver f
scalarA name f fld@(Field _ _ args []) = withField name (f args) fld
scalarA _ _ _ = empty

array :: Alternative f => Name -> [Resolvers f] -> Resolver f
array name resolvers = arrayA name $ \case
    [] -> resolvers
    _  -> empty

-- | Like 'array' but also taking 'Argument's.
arrayA
  :: Alternative f
  => Text -> (Arguments -> [Resolvers f]) -> Resolver f
arrayA name f fld@(Field _ _ args sels) =
     withField name (traverse (`resolve` sels) $ f args) fld

-- | Like 'object'' but taking lists of 'Resolver's instead of a single list.
array' :: (Alternative f, Monad f) => Text -> f [[Resolver f]] -> Resolver f
array' name resolvs = arrayA' name $ \case
    [] -> resolvs
    _  -> empty

-- | Like 'array'' but also taking 'Argument's.
arrayA'
  :: (Alternative f, Monad f)
  => Text -> ([Argument] -> f [[Resolver f]]) -> Resolver f
arrayA' name f fld@(Field _ _ args sels) = do
     resolvs <- f args
     withField name (traverse (`resolve` sels) resolvs) fld

-- | Represents one of a finite set of possible values.
--   Used in place of a 'scalar' when the possible responses are easily enumerable.
enum :: Alternative f => Text -> f [Text] -> Resolver f
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

-- | Like 'enum' but also taking 'Argument's.
enumA :: Alternative f => Text -> ([Argument] -> f [Text]) -> Resolver f
enumA name f fld@(Field _ _ args []) = withField name (f args) fld
enumA _ _ _ = empty

-- | Helper function to facilitate 'Argument' handling.
withField
  :: (Alternative f, Aeson.ToJSON a)
  => Name -> f a -> Field -> f (HashMap Text Aeson.Value)
withField name v (Field alias name' _ _) =
  if name == name'
    then fmap (HashMap.singleton aliasOrName . Aeson.toJSON) v
         -- TODO: Report error when Non-Nullable type for field argument.
     <|> pure (HashMap.singleton aliasOrName Aeson.Null)
    else empty
  where
    aliasOrName = fromMaybe name alias

-- | Takes a list of 'Resolver's and a list of 'Field's and applies each
--   'Resolver' to each 'Field'. Resolves into a value containing the
--   resolved 'Field', or a null value and error information.
resolve :: Alternative f => Resolvers f -> Fields -> f Aeson.Value
resolve resolvers =
    fmap (Aeson.toJSON . fold)
  . traverse (\fld -> getAlt (foldMap (Alt . ($ fld)) resolvers))
