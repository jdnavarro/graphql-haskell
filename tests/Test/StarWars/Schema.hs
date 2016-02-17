{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema where

import Control.Applicative ((<|>), Alternative, empty)

import Data.GraphQL.Schema

import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: (Alternative m, Monad m) => Schema m
schema = Schema query

query :: (Alternative m, Monad m) => ResolverM m
query fld =
      withField "hero"  hero  fld
  <|> withField "human" human fld
  <|> withField "droid" droid fld

hero :: Alternative f => [Argument] -> ResolverO f
hero [] = characterFields artoo
hero args =
  case withArgument "episode" args of
       Just (ScalarInt n) -> characterFields $ getHero n
       _ -> const empty

human :: (Alternative m, Monad m) => [Argument] -> ResolverO m
human args flds =
  case withArgument "id" args of
       Just (ScalarString i) -> flip characterFields flds =<< getHuman i
       _ -> empty

droid :: (Alternative m, Monad m) => [Argument] -> ResolverO m
droid args flds =
   case withArgument "id" args of
        Just (ScalarString i) -> flip characterFields flds =<< getDroid i
        _ -> empty

characterField :: Alternative f => Character -> ResolverM f
characterField char fld =
      withFieldFinal "id"        (OutputScalar . ScalarString . id_ $ char) fld
  <|> withFieldFinal "name"      (OutputScalar . ScalarString . name $ char) fld
  <|> withField      "friends"   friends' fld
  <|> withField      "appearsIn" appears' fld
  where
    friends' [] flds = outputTraverse (`characterFields` flds) $ getFriends char
    friends' _ _ = empty

    appears' [] [] = outputTraverse (fmap OutputEnum . getEpisode) $ appearsIn char
    appears' _ _ = empty

characterFields :: Alternative f => Character -> ResolverO f
characterFields = withFields . characterField
