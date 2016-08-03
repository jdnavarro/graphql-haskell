{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds, PolyKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}

module Test.StarWars.Schema where

import Control.Applicative (Alternative, empty)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif
import Control.Applicative ((<|>))
import qualified Data.GraphQL.Schema as Schema
import Data.GraphQL.ServantSchema
import Data.Proxy
import qualified Data.Text as T
import Data.Text (Text)

import Test.StarWars.Data

import Prelude hiding (Enum)

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

type CharacterSchema = Const "id" ID
									:<|> Const "name" Text
								  :<|> Enum "appearsIn" Text
                  :<|> Const "secretBackstory" Text

type CharacterWithFriendsSchema = CharacterSchema
													   :<|> Array "friends" :> CharacterSchema

type StarWarsSchema = Object "hero" :> CharacterWithFriendsSchema
                 :<|> ArgNotNull "id" ID :> Object "human" :> CharacterWithFriendsSchema

schemaImpl :: (Alternative f, Monad f) => Schema f StarWarsSchema
schemaImpl = hero :<|> human

hero _ = characterWithFriends $ pure artoo

human :: (Alternative f, Monad f) => ID -> Text -> Schema f CharacterWithFriendsSchema
human id _ = characterWithFriends $ getHuman id

characterWithFriends :: (Alternative f, Monad f) => Maybe Character -> Schema f CharacterWithFriendsSchema
characterWithFriends (Just char) = character (Just char) :<|> friends'
	where friends' = character . Just <$> getFriends char
characterWithFriends Nothing = character Nothing :<|> empty

character :: (Alternative f, Monad f) => Maybe Character -> Schema f CharacterSchema
character (Just char) = pure (id_ char) :<|> pure (name char) :<|> traverse getEpisode (appearsIn char) :<|> pure (secretBackstory char)
character Nothing = empty :<|> empty :<|> empty :<|> empty

schema :: (Alternative f, Monad f) => Schema.Schema f
schema = Schema.Schema [convert (Proxy :: Proxy StarWarsSchema) schemaImpl]
