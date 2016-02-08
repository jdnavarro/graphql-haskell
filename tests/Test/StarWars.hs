{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), pure)
import Data.Monoid (mempty)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, (<|>), empty, liftA2)
import Data.Maybe (catMaybes)

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Text.RawString.QQ (r)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL
import Data.GraphQL.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testGroup "Basic Queries"
  [ testCase "R2-D2 hero" $ (@?=) (graphql schema [r|
query HeroNameQuery {
  hero {
    id
  }
}|]) . Just
     $ object [
         "hero" .= object [
              "id"   .= ("2001" :: Text)
            ]
         ]

  , testCase "R2-D2 ID and friends" $ (@?=) (graphql schema [r|
query HeroNameAndFriendsQuery {
  hero {
    id
    name
    friends {
      name
    }
  }
}|]) . Just
     $ object [
          "hero" .= object [
               "id" .= ("2001" :: Text)
             , "name" .= ("R2-D2" :: Text)
             , "friends" .= [
                    object ["name" .= ("Luke Skywalker" :: Text)]
                  , object ["name" .= ("Han Solo" :: Text)]
                  , object ["name" .= ("Leia Organa" :: Text)]
                  ]
             ]
           ]
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
    withFields inputFields <$> getHero ep
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

vader :: Character
vader = Character
  { id_        = "1001"
  , name       = "Darth Vader"
  , friends    = ["1004"]
  , appearsIn  = [4,5,6]
  , homePlanet = "Tatooine"
  }

han :: Character
han = Character
  { id_        = "1002"
  , name       = "Han Solo"
  , friends    = ["1000","1003","2001" ]
  , appearsIn  = [4,5,6]
  , homePlanet = mempty
  }

leia :: Character
leia = Character
  { id_        = "1003"
  , name       = "Leia Organa"
  , friends    = ["1000","1002","2000","2001"]
  , appearsIn  = [4,5,6]
  , homePlanet = "Alderaan"
  }

tarkin :: Character
tarkin = Character
  { id_        = "1004"
  , name       = "Wilhuff Tarkin"
  , friends    = ["1001"]
  , appearsIn  = [4]
  , homePlanet = mempty
  }

threepio :: Character
threepio = Character
  { id_ = "2000"
  , name = "C-3PO"
  , friends = ["1000","1002","1003","2001" ]
  , appearsIn = [ 4, 5, 6 ]
  , homePlanet = "Protocol"
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
getHuman "1001" = pure vader
getHuman "1002" = pure han
getHuman "1003" = pure leia
getHuman "1004" = pure tarkin
getHuman _      = empty

getDroid :: Alternative f => ID -> f Character
getDroid "2000" = pure threepio
getDroid "2001" = pure artoo
getDroid _      = empty

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char
