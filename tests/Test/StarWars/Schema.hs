{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema where

import Control.Applicative (Alternative, empty)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Traversable (traverse)
#endif
import Data.GraphQL.Schema
import qualified Data.GraphQL.Schema as Schema

import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: Alternative f => Schema f
schema = Schema [hero, human, droid]

hero :: Alternative f => Resolver f
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Argument "episode" (ValueInt n)] -> character $ getHero (fromIntegral n)
  _ -> empty

human :: Alternative f => Resolver f
human = Schema.objectA "human" $ \case
  [Argument "id" (ValueString (StringValue i))] -> character =<< getHuman i
  _ -> empty

droid :: Alternative f => Resolver f
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString (StringValue i))] -> character =<< getDroid i
   _ -> empty

character :: Alternative f => Character -> [Resolver f]
character char =
  [ Schema.scalar "id"        $ id_ char
  , Schema.scalar "name"      $ name char
  , Schema.array  "friends"   $ character <$> getFriends char
  , Schema.enum   "appearsIn" . traverse getEpisode $ appearsIn char
  ]
