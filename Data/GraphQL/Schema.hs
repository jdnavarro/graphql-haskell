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
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Data.Monoid (Monoid(mempty,mappend))
#else
import Data.Monoid (Alt(Alt,getAlt))
#endif
import Control.Applicative ((<|>),pure,empty)
import Data.Maybe (catMaybes)
import Data.Foldable (fold)

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T (null, unwords)


import Data.GraphQL.AST

import Control.Monad (MonadPlus)
import Data.GraphQL.Error

data Schema m = Schema [Resolver m]

type Resolver m = Field -> CollectErrsT m Aeson.Object

type Subs = Text -> Maybe Text


object :: MonadPlus m => Text -> [Resolver m] -> Resolver m
object name resolvs = objectA name $ \case
     [] -> resolvs
     _  -> empty

objectA
  :: MonadPlus m => Text -> ([Argument] -> [Resolver m]) -> Resolver m
objectA name f fld@(Field _ _ args _ sels) =
    withField name (resolvers (f args) $ fields sels) fld

scalar :: (MonadPlus m,  Aeson.ToJSON a) => Text -> a -> Resolver m
scalar name s = scalarA name $ \case
    [] -> pure s
    _  -> empty

scalarA
  :: (MonadPlus m, Aeson.ToJSON a)
  => Text -> ([Argument] -> m a) -> Resolver m
scalarA name f fld@(Field _ _ args _ []) = withField name (lift $ f args) fld
scalarA _ _ _ = empty

array :: MonadPlus m => Text -> [[Resolver m]] -> Resolver m
array name resolvs = arrayA name $ \case
    [] -> resolvs
    _  -> empty

arrayA :: MonadPlus m => Text -> ([Argument] -> [[Resolver m]]) -> Resolver m
arrayA name f fld@(Field _ _ args _ sels) =
     withField name (traverse (flip resolvers $ fields sels) $ f args) fld

enum :: MonadPlus m => Text -> m [Text] -> Resolver m
enum name enums = enumA name $ \case
     [] -> enums
     _  -> empty

enumA :: MonadPlus m => Text -> ([Argument] -> m [Text]) -> Resolver m
enumA name f fld@(Field _ _ args _ []) = withField name (lift $ f args) fld
enumA _ _ _ = empty

withField :: (MonadPlus m, Aeson.ToJSON a)
             => Text -> CollectErrsT m a
             -> Field -> CollectErrsT m (HashMap Text Aeson.Value)
withField name f (Field alias name' _ _ _) =
     if name == name'
        then HashMap.singleton aliasOrName . Aeson.toJSON  <$> runAppendErrs f
        else empty
     where
       aliasOrName = if T.null alias then name' else alias


resolvers :: MonadPlus m => [Resolver m] -> [Field] -> CollectErrsT m Aeson.Value
resolvers resolvs =
    fmap (Aeson.toJSON . fold)
  . traverse (\fld -> (getAlt $ foldMap (Alt . ($ fld)) resolvs) <|> errmsg fld)
    where errmsg (Field alias name _ _ _) =
            do addErrMsg $ T.unwords ["field", name, "not resolved."]
               return $ HashMap.singleton aliasOrName Aeson.Null
            where aliasOrName = if T.null alias then name else alias

field :: Selection -> Maybe Field
field (SelectionField x) = Just x
field _ = Nothing

fields :: SelectionSet -> [Field]
fields = catMaybes . fmap field

#if !MIN_VERSION_base(4,8,0)
newtype Alt f a = Alt {getAlt :: f a}

instance MonadPlus f => Monoid (Alt f a) where
        mempty = Alt empty
        Alt x `mappend` Alt y = Alt $ x <|> y
#endif
