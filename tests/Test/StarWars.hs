{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), pure)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, (<|>), empty, liftA2)
import Data.Maybe (catMaybes)

import Data.Aeson (object, (.=))
import Data.Text (Text)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL
import Data.GraphQL.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testGroup "Basic Queries"
     [testCase "R2-D2"
         $  graphql schema "query HeroNameQuery{hero{name}}"
        @?= Just (object ["hero" .= object ["name" .= ("R2-D2" :: Text)]])
     ]

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

type ID = Text

schema :: Alternative f => Schema f
schema = Schema query

query :: Alternative f => QueryRoot f
query (InputField "hero")  = pure $ OutputResolver hero
query (InputField "human") = pure $ OutputResolver human
query (InputField "droid") = pure $ OutputResolver droid
query _ = empty

hero :: Alternative f => Resolver f
hero (InputList (InputScalar (ScalarInt ep) : inputFields)) =
    withFields inputFields  <$> getHero ep
hero (InputField fld) = characterOutput fld artoo
hero _ = empty

human :: Alternative f => Resolver f
human (InputList (InputScalar (ScalarID i) : inputFields)) =
    withFields inputFields <$> getHuman i
human _ = empty

droid :: Alternative f => Resolver f
droid (InputList (InputScalar (ScalarID i) : inputFields)) =
    withFields inputFields <$> getDroid i
droid _ = empty

characterOutput :: Alternative f => Text -> Character -> f (Output f)
characterOutput "id" char =
    pure $ OutputScalar . pure . ScalarString $ id_  char
characterOutput "name" char =
    pure $ OutputScalar . pure . ScalarString $ name char
characterOutput "friends" char =
    -- TODO: Cleanup
    pure . OutputList . pure $ OutputResolver . (\c (InputField f) ->
      characterOutput f c) <$> getFriends char
characterOutput _ _ = empty

withFields :: Alternative f => [Input] -> Character -> Output f
withFields inputFields char =
    OutputList . traverse (`characterOutput` char) $ fields inputFields

-- * Data
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsData.js

-- ** Characters

data Character = Character
  { id_        :: ID
  , name       :: Text
  , friends    :: [ID]
  , appearsIn  :: [Int]
  , homePlanet :: Text
  } deriving (Show)

luke :: Character
luke = Character
  { id_        = "1000"
  , name       = "Luke Skywalker"
  , friends    = ["1002","1003","2000","2001"]
  , appearsIn  = [4,5,6]
  , homePlanet = "Tatoonie"
  }

artoo :: Character
artoo = Character
  { id_        = "2001"
  , name       = "R2-D2"
  , friends    = ["1000","1002","1003"]
  , appearsIn  = [4,5,6]
  , homePlanet = "Astrometch"
  }

-- ** Helper functions

getHero :: Applicative f => Int -> f Character
getHero 5 = pure luke
getHero _ = pure artoo

getHeroIO :: Int -> IO Character
getHeroIO = getHero

getHuman :: Alternative f => ID -> f Character
getHuman "1000" = pure luke
-- getHuman "1001" = "vader"
-- getHuman "1002" = "han"
-- getHuman "1003" = "leia"
-- getHuman "1004" = "tarkin"
getHuman _      = empty

getDroid :: Alternative f => ID -> f Character
-- getDroid "2000" = "threepio"
getDroid "2001" = pure artoo
getDroid _ = empty

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char
