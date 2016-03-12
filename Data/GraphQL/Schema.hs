{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
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

data Schema f = Schema [Resolver f]

type Resolver f = Field -> CollectErrsT f Aeson.Object

type Subs = Text -> Maybe Text

object :: Alternative f => Text -> [Resolver f] -> Resolver f
object name resolvs = objectA name $ \case
     [] -> resolvs
     _  -> empty

objectA
  :: Alternative f
  => Text -> ([Argument] -> [Resolver f]) -> Resolver f
objectA name f fld@(Field _ _ args _ sels) =
    withField name (resolvers (f args) $ fields sels) fld

scalar :: (Alternative f, Aeson.ToJSON a) => Text -> a -> Resolver f
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> empty

scalarA
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> ([Argument] -> f a) -> Resolver f
scalarA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
scalarA _ _ _ = empty

array :: Alternative f => Text -> [[Resolver f]] -> Resolver f
array name resolvs = arrayA name $ \case
    [] -> resolvs
    _  -> empty

arrayA
  :: Alternative f
  => Text -> ([Argument] -> [[Resolver f]]) -> Resolver f
arrayA name f fld@(Field _ _ args _ sels) =
     withField name (joinErrs $ traverse (flip resolvers $ fields sels) $ f args) fld

enum :: Alternative f => Text -> f [Text] -> Resolver f
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

enumA :: Alternative f => Text -> ([Argument] -> f [Text]) -> Resolver f
enumA name f fld@(Field _ _ args _ []) = withField name (errWrap $ f args) fld
enumA _ _ _ = empty

withField
  :: (Alternative f, Aeson.ToJSON a)
  => Text -> CollectErrsT f a -> Field -> CollectErrsT f (HashMap Text Aeson.Value)
withField name f (Field alias name' _ _ _) =
     if name == name'
        then fmap (first $ HashMap.singleton aliasOrName . Aeson.toJSON) f
        else empty
     where
       aliasOrName = if T.null alias then name' else alias

resolvers :: Alternative f => [Resolver f] -> [Field] -> CollectErrsT f Aeson.Value
resolvers resolvs =
    fmap (first Aeson.toJSON . fold)
  . traverse (\fld -> (getAlt $ foldMap (Alt . ($ fld)) resolvs) <|> errmsg fld)
    where errmsg (Field alias name _ _ _) = addErrMsg msg $ (errWrap . pure) val
            where val = HashMap.singleton aliasOrName Aeson.Null
                  msg = T.unwords ["field", name, "not resolved."]
                  aliasOrName = if T.null alias then name else alias

field :: Selection -> Maybe Field
field (SelectionField x) = Just x
field _ = Nothing

fields :: SelectionSet -> [Field]
fields = catMaybes . fmap field

#if !MIN_VERSION_base(4,8,0)
newtype Alt f a = Alt {getAlt :: f a}

instance Alternative f => Monoid (Alt f a) where
        mempty = Alt empty
        Alt x `mappend` Alt y = Alt $ x <|> y
#endif
