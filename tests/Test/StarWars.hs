{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Test.StarWars where

import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)
import Data.GraphQL.Schema

type ID = Text

schema :: Applicative f => Schema f
schema = Schema query Nothing

query :: Applicative f => QueryRoot f
query = [ ("hero", hero)
        , ("human", human)
        , ("droid", droid)
        ]

hero :: Applicative f => Input -> f Output
hero (InputScalar (ScalarInt ep)) = OutputObject <$> getHeroF ep
hero _ = pure InputError

human :: Applicative f => Input -> f Output
human (InputScalar (ScalarString id_)) = OutputScalar . ScalarString <$> getHumanF id_
human _ = pure InputError

droid :: Applicative f => Input -> f Output
droid (InputScalar (ScalarString id_)) = OutputScalar . ScalarString <$> getDroidF id_
droid _ = pure InputError

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

type CharacterObject = HashMap Text Output

character :: Character -> CharacterObject
character (Character{..}) =
  [ ("id_", OutputScalar $ ScalarID id_)
  , ("name", OutputScalar $ ScalarString name)
  , ("friends", OutputList $ OutputScalar . ScalarID <$> friends)
  , ("appearsIn", OutputList $ OutputScalar . ScalarInt <$> appearsIn)
  , ("homePlanet", OutputScalar $ ScalarString homePlanet)
  ]

-- ** Hero

getHero :: Int -> CharacterObject
getHero 5 = character luke
getHero _ = character artoo

getHeroF :: Applicative f => Int -> f CharacterObject
getHeroF = pure . getHero

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
