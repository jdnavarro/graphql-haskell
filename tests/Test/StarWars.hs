{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Test.StarWars where

import Data.Functor.Identity (Identity(..))
import Data.Text (Text)

import Data.Attoparsec.Text (parseOnly)
import qualified Data.Aeson as Aeson

import Test.Tasty (TestTree)
import Test.Tasty.HUnit

import Data.GraphQL.AST
import Data.GraphQL.Execute
import qualified Data.GraphQL.Parser as Parser
import Data.GraphQL.Schema

-- * Test

test :: TestTree
test = testCase "R2-D2" $ execute schema heroQuery @=? Identity expected
  where
    heroQuery :: Document
    heroQuery = either (error "Parsing error") id $ parseOnly Parser.document
      "{ query HeroNameQuery { hero { name } } }"

    expected :: Response
    expected = Aeson.Object
        [ ( "hero" , Aeson.Object [ ("name", "R2-D2") ] ) ]

-- * Schema

type ID = Text

schema :: Applicative f => Schema f
schema = Schema query Nothing

query :: Applicative f => QueryRoot f
query = [ ("hero", hero)
        , ("human", human)
        , ("droid", droid)
        ]

hero :: Applicative f => Resolver f
hero (InputScalar (ScalarInt ep)) = OutputMap $ getHeroF ep
hero _ = InputError

human :: Applicative f => Resolver f
human (InputScalar (ScalarString id_)) = OutputScalar $ ScalarString <$> getHumanF id_
human _ = InputError

droid :: Applicative f => Resolver f
droid (InputScalar (ScalarString id_)) = OutputScalar $ ScalarString <$> getDroidF id_
droid _ = InputError

-- * Data

-- ** Characters

data Character = Character
  { id_ :: ID
  , name :: Text
  , friends :: [ID]
  , appearsIn :: [Int]
  , homePlanet :: Text
  }

luke :: Character
luke = Character
  { id_ = "1000"
  , name = "Luke Skywalker"
  , friends = ["1002","1003","2000","2001"]
  , appearsIn = [4,5,6]
  , homePlanet = "Tatoonie"
  }

artoo :: Character
artoo = Character
  { id_ = "2001"
  , name = "R2-D2"
  , friends = ["1000","1002","1003"]
  , appearsIn = [4,5,6]
  , homePlanet = "Astrometch"
  }

type CharacterMap f = Map f

character :: Applicative f => Character -> CharacterMap f
character (Character{..}) =
  [ ("id_", const . OutputScalar . pure $ ScalarID id_)
  , ("name", const . OutputScalar . pure $ ScalarString name)
  , ("friends", const . OutputList $ OutputScalar . pure . ScalarID <$> friends)
  , ("appearsIn", const . OutputList $ OutputScalar . pure . ScalarInt <$> appearsIn)
  , ("homePlanet", const . OutputScalar . pure $ ScalarString homePlanet)
  ]

-- ** Hero

getHero :: Int -> Character
getHero 5 = luke
getHero _ = artoo

getHeroF :: Applicative f => Int -> CharacterMap f
getHeroF = character . getHero

-- ** Human

getHuman :: ID -> Text
getHuman "1000" = "luke"
getHuman "1001" = "vader"
getHuman "1002" = "han"
getHuman "1003" = "leia"
getHuman "1004" = "tarkin"
getHuman _      = ""

getHumanF :: Applicative f => ID -> f Text
getHumanF = pure . getHuman

getHumanIO :: ID -> IO Text
getHumanIO = getHumanF

-- ** Droid

getDroid :: ID -> Text
getDroid "2000" = "threepio"
getDroid "2001" = "artoo"
getDroid _ = ""

getDroidF :: Applicative f => ID -> f Text
getDroidF = pure . getDroid

getDroidIO :: ID -> IO Text
getDroidIO = getDroidF
