{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.StarWars.Schema where


import Data.List.NonEmpty (NonEmpty((:|)))

import Control.Exception.Safe.Checked (MonadCatch, Throws, throw)


import Data.GraphQL.Schema (Schema, Resolver, Argument(..), Value(..))
import Data.GraphQL.Error (NotFound(..))
import qualified Data.GraphQL.Schema as Schema

import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: (MonadCatch m, Throws NotFound) => Schema m
schema = hero :| [human, droid]

hero :: (MonadCatch m, Throws NotFound) => Resolver m
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Argument "episode" (ValueInt n)]   -> character . getHero $ fromIntegral n
  [Argument "episode" (ValueEnum "NEWHOPE")] -> character $ getHero 4
  [Argument "episode" (ValueEnum "EMPIRE" )] -> character $ getHero 5
  [Argument "episode" (ValueEnum "JEDI"   )] -> character $ getHero 6
  _ -> throw NotFound

human :: (MonadCatch m, Throws NotFound) => Resolver m
human = Schema.objectA "human" $ \case
  [Argument "id" (ValueString i)] -> maybe (throw NotFound) character (getHuman i)
  _ -> throw NotFound

droid :: (MonadCatch m, Throws NotFound) => Resolver m
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString i)] -> maybe (throw NotFound) character (getDroid i)
   _ -> throw NotFound

character :: (MonadCatch m, Throws NotFound) => Character -> [Resolver m]
character char =
  [ Schema.scalar "id"              $ id_ char
  , Schema.scalar "name"            $ name char
  , Schema.array  "friends"         $ character <$> getFriends char
  , Schema.enum   "appearsIn"       . traverse (maybe (throw NotFound) pure . getEpisode) $ appearsIn char
  -- , Schema.scalar "secretBackstory" $ secretBackstory char
  , Schema.scalar "homePlanet"      $ either mempty homePlanet char
  , Schema.scalar "__typename"      $ typeName char
  ]
