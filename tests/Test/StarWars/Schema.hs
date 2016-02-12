{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Schema where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, empty)
import Data.Foldable (fold)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)

import Data.GraphQL.Schema

import Test.StarWars.Data

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

schema :: (Alternative m, Monad m) => Schema m
schema = Schema query

query :: (Alternative m, Monad m) => QueryRoot m
query (InputField "hero"  args ins) = hero  args ins
query (InputField "human" args ins) = human args ins
query (InputField "droid" args ins) = droid args ins
query _ = empty

hero :: Alternative f => [Argument] -> [Input] -> f Output
hero [] = characterFields artoo
hero [("episode", ScalarInt n)] = characterFields $ getHero n
hero _ = const empty

human :: (Alternative m, Monad m) => [Argument] -> [Input] -> m Output
human [("id", ScalarString i)] ins = flip characterFields ins =<< getHuman i
human _ _ = empty

droid :: (Alternative m, Monad m) => [Argument] -> [Input] -> m Output
droid [("id", ScalarString i)] ins = flip characterFields ins =<< getDroid i
droid _ _ = empty

episode :: Alternative f => Int -> f Output
episode 4 = pure $ OutputEnum "NEWHOPE"
episode 5 = pure $ OutputEnum "EMPIRE"
episode 6 = pure $ OutputEnum "JEDI"
episode _ = empty

characterField :: Alternative f => Character -> Input -> f (HashMap Text Output)
characterField char (InputField "id" [] [])  =
   pure . HashMap.singleton "id" . OutputScalar . ScalarString . id_ $ char
characterField char (InputField "name" [] []) =
   pure . HashMap.singleton "name" . OutputScalar . ScalarString . name $ char
characterField char (InputField "friends" [] ins) =
     fmap (HashMap.singleton "friends" . OutputList)
   . traverse (`characterFields` ins)
   . getFriends
   $ char
characterField char (InputField "appearsIn" [] []) =
     fmap (HashMap.singleton "appearsIn" . OutputList)
   . traverse episode
   . appearsIn
   $ char
characterField _ _ = empty

characterFields :: Alternative f => Character -> [Input] -> f Output
characterFields char = fmap (OutputObject . fold) . traverse (characterField char)
