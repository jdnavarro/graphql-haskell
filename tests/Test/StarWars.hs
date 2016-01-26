{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Applicative ((<|>), liftA2)
import Data.Maybe (catMaybes)
import Data.Text (Text)

import qualified Data.Aeson as Aeson
import Data.Attoparsec.Text (parseOnly)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Data.GraphQL.AST
import Data.GraphQL.Execute
import qualified Data.GraphQL.Parser as Parser
import Data.GraphQL.Schema


-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testCase "R2-D2" $ execute schema heroQuery @?= expected
  where
    heroQuery :: Document
    heroQuery = either (error "Parsing error") id $ parseOnly Parser.document
      "query HeroNameQuery{hero{name}}"

    expected :: Maybe Response
    expected = Just $ Aeson.Object [("hero", Aeson.Object [("name", "R2-D2")])]

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

type ID = Text

schema :: Schema
schema = Schema query

query ::  QueryRoot
query (InputField "hero")  = OutputResolver hero
query (InputField "human") = OutputResolver human
query (InputField "droid") = OutputResolver droid
query _ = OutputError

-- TODO: Extract helper function from next 3 functions.

hero :: Resolver
hero (InputList (InputScalar (ScalarInt ep) : inputFields)) =
    maybe OutputError (\char -> OutputList $ (`characterOutput` char) <$> fields inputFields) $ getHero ep
hero (InputField fld) = characterOutput fld artoo
hero _ = OutputError

human :: Resolver
human (InputList (InputScalar (ScalarID i) : inputFields)) =
    maybe OutputError (\char -> OutputList $ (`characterOutput` char) <$> fields inputFields) $ getHuman i
human _ = OutputError

droid :: Resolver
droid (InputList (InputScalar (ScalarID i) : inputFields)) =
    maybe OutputError (\char -> OutputList $ (`characterOutput` char) <$> fields inputFields) $ getDroid i
droid _ = OutputError

characterOutput :: Text -> Character -> Output
characterOutput "id"      char = OutputScalar . ScalarString $ id_  char
characterOutput "name"    char = OutputScalar . ScalarString $ name char
characterOutput "friends" char = OutputList $ OutputResolver . (\c (InputField f) -> characterOutput f c) <$> getFriends char
characterOutput _ _ = OutputError

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

getHero :: Int -> Maybe Character
getHero 5 = Just luke
getHero _ = Just artoo

getHuman :: ID -> Maybe Character
getHuman "1000" = Just luke
-- getHuman "1001" = "vader"
-- getHuman "1002" = "han"
-- getHuman "1003" = "leia"
-- getHuman "1004" = "tarkin"
getHuman _      = Nothing

getDroid :: ID -> Maybe Character
-- getDroid "2000" = "threepio"
getDroid "2001" = Just artoo
getDroid _ = Nothing


getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char
