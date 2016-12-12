{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

-- | This module provides a representation of a @GraphQL@ Schema in addition to
--   functions for defining and manipulating Schemas.
module Data.GraphQL.ServantSchema
  ( Arg
  , ArgNotNull
	, Array
  , Object
  , Const
  , Enum
  , Schema
  , convert
  , (:>)
  , (:<|>) (..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Control.Arrow (first)
import Data.Foldable (foldMap)
import Data.Traversable (traverse)
import Data.Monoid (Monoid(mempty,mappend), (<>))
#else
import Data.Bifunctor (first)
import Data.Monoid (Alt(Alt,getAlt), (<>))
#endif
import Control.Applicative (Alternative((<|>), empty))
import Data.Proxy
import Data.Maybe (catMaybes)
import Data.Foldable (fold, find)

import qualified Data.Aeson as Aeson
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as T (null, unwords, pack, unpack)

import Data.GraphQL.AST
import Data.GraphQL.Error
import qualified Data.GraphQL.Schema as S

import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Debug.Trace
import Prelude hiding (Enum)

data Resolver (a :: *)

data a :<|> b = a :<|> b
infixr 8 :<|>

data (a :: k) :> (b :: *)
infixr 9 :>

data Const (s :: Symbol) (a :: *)
data Enum (s :: Symbol) (a :: *)
data Object (s :: Symbol)
data Arg (s :: Symbol) (a :: *)
data ArgNotNull (s :: Symbol) (a :: *)
data Array (s :: Symbol)

type family Schema (f :: * -> *) (layout :: *)

type instance Schema f (Const s a) = f a
type instance Schema f (Enum s a) = f [a]

type instance Schema f (a :<|> b) = Schema f a :<|> Schema f b

type instance Schema f (Object s :> r) = Text -> Schema f r
type instance Schema f (Array s :> r) = [Schema f r]
type instance Schema f (Arg s a :> r) = Maybe a -> Schema f r
type instance Schema f (ArgNotNull s a :> r) = a -> Schema f r

class HasSchema layout where
  path :: Alternative f => Proxy layout -> Schema f layout -> S.Resolver f

convert :: (HasSchema layout, Alternative f)
        => Proxy layout -> Schema f layout -> S.Resolver f
convert p h = path p h

instance (KnownSymbol s, Aeson.ToJSON a) => HasSchema (Const s a) where
  path p handler = S.scalarA (T.pack $ symbolVal (Proxy :: Proxy s)) (const handler)

instance (KnownSymbol s, Aeson.ToJSON a) => HasSchema (Enum s a) where
  path :: forall f. Alternative f => Proxy (Enum s a) -> f [a] -> S.Resolver f
  path p handler = newHandler -- S.enumA name (const handler)
    where resolvers :: f [a]
          resolvers = handler
          m :: CollectErrsT f [a]
          m = errWrap resolvers
          newHandler :: S.Resolver f
          newHandler fld@(Field _ _ args _ []) = S.withField name m fld
          newHandler _ = empty
          name = T.pack $ symbolVal (Proxy :: Proxy s)

instance (HasSchema a, HasSchema b) => HasSchema (a :<|> b) where
  path :: forall f. Alternative f => Proxy (a :<|> b) -> (Schema f a :<|> Schema f b) -> S.Resolver f
  path p (handlerA :<|> handlerB) = newHandler
    where
      newHandler fld@(Field falias fname args _ _) = a' fld <|> b' fld
      a' = path (Proxy :: Proxy a) handlerA
      b' = path (Proxy :: Proxy b) handlerB

class Argumentable v where
  fromArgument :: Value -> Maybe v

instance Argumentable String where
  fromArgument (ValueString t) = Just $ T.unpack t
  fromArgument _ = Nothing

instance Argumentable Text where
  fromArgument (ValueString t) = Just t
  fromArgument _ = Nothing

instance Argumentable Int where
  fromArgument (ValueInt n) = Just $ fromIntegral n
  fromArgument _ = Nothing

instance (KnownSymbol s, HasSchema r, Argumentable a) => HasSchema (Arg (s :: Symbol) a :> r) where
  path :: forall f. Alternative f => Proxy (Arg s a :> r) -> (Maybe a -> Schema f r) -> S.Resolver f
  path _ handler = newHandler
    where name = T.pack $ symbolVal (Proxy :: Proxy s)
          newHandler :: S.Resolver f
          newHandler fld@(Field falias fname args _ _) =
            let
              arg :: Maybe a
              arg = find (\(Argument argName _) -> argName == name) args >>= (\(Argument _ val) -> Just val) >>= fromArgument
              resolver :: S.Resolver f
              resolver = path (Proxy :: Proxy r) (handler arg)
            in resolver fld

instance (KnownSymbol s, HasSchema r, Argumentable a) => HasSchema (ArgNotNull (s :: Symbol) a :> r) where
  path :: forall f. Alternative f => Proxy (ArgNotNull s a :> r) -> (a -> Schema f r) -> S.Resolver f
  path _ handler = newHandler
    where name = T.pack $ symbolVal (Proxy :: Proxy s)
          newHandler :: S.Resolver f
          newHandler fld@(Field falias fname args _ _) = case find (\(Argument argName _) -> argName == name) args of
            Just (Argument name val) -> case fromArgument val of
              Just x ->
                let
                  resolver :: S.Resolver f
                  resolver = path (Proxy :: Proxy r) (handler x)
                  aliasOrName = if T.null falias then fname else falias
                in resolver fld
              Nothing -> empty
            Nothing -> empty

instance (KnownSymbol s, HasSchema r) => HasSchema (Object (s :: Symbol) :> r) where
  path _ handler = S.objectA name (\args -> [objResolver])
    where objResolver = path (Proxy :: Proxy r) $ handler $ T.pack $ symbolVal (Proxy :: Proxy s)
          name = T.pack $ symbolVal (Proxy :: Proxy s)

instance (KnownSymbol s, HasSchema r) => HasSchema (Array (s :: Symbol) :> r) where
  path :: forall f. Alternative f => Proxy (Array s :> r) -> [Schema f r] -> S.Resolver f
  path _ handler = newHandler
    where resolvers :: [S.Resolver f]
          resolvers = path (Proxy :: Proxy r) <$> handler
          newHandler :: S.Resolver f
          newHandler fld@(Field alias name args _ sels) =
            fmap (first $ HashMap.singleton name . Aeson.toJSON) $ joinErrs $ traverse (\a -> S.resolvers [a] $ S.fields sels) resolvers
          name = T.pack $ symbolVal (Proxy :: Proxy s)
