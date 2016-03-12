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
import Control.Monad

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: MonadPlus f => Schema f
schema = Schema [hero, human, droid]

hero :: MonadPlus f => Resolver f
hero = Schema.objectA "hero" $ \case
  [] -> character artoo
  [Argument "episode" (ValueInt n)] -> character $ getHero (fromIntegral n)
  _ -> empty

human :: MonadPlus f => Resolver f
human = Schema.objectA "human" $ \case
  [Argument "id" (ValueString i)] -> character =<< getHuman i
  _ -> empty

droid :: MonadPlus f => Resolver f
droid = Schema.objectA "droid" $ \case
   [Argument "id" (ValueString i)] -> character =<< getDroid i
   _ -> empty

character :: MonadPlus f => Character -> [Resolver f]
character char =
  [ Schema.scalar "id"        $ id_ char
  , Schema.scalar "name"      $ name char
  , Schema.array  "friends"   $ character <$> getFriends char
  , Schema.enum   "appearsIn" . traverse getEpisode $ appearsIn char
  ]
