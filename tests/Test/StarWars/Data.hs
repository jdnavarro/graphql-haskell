{-# LANGUAGE OverloadedStrings #-}
module Test.StarWars.Data where

import Data.Monoid (mempty)
import Control.Applicative ((<|>), liftA2)
import Data.Maybe (catMaybes)

import Data.Text (Text)

-- * Data
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsData.js

-- ** Characters

type ID = Text

data CharCommon = CharCommon
  { _id_        :: ID
  , _name       :: Text
  , _friends    :: [ID]
  , _appearsIn  :: [Int]
  } deriving (Show)


data Human = Human
  { _humanChar  :: CharCommon
  , homePlanet :: Text
  }

data Droid = Droid
  { _droidChar       :: CharCommon
  , primaryFunction :: Text
  }

type Character = Either Droid Human

id_ :: Character -> ID
id_ (Left  x) = _id_ . _droidChar $ x
id_ (Right x) = _id_ . _humanChar $ x

name :: Character -> Text
name (Left  x) = _name . _droidChar $ x
name (Right x) = _name . _humanChar $ x

friends :: Character -> [ID]
friends (Left  x) = _friends . _droidChar $ x
friends (Right x) = _friends . _humanChar $ x

appearsIn :: Character -> [Int]
appearsIn (Left  x) = _appearsIn . _droidChar $ x
appearsIn (Right x) = _appearsIn . _humanChar $ x

secretBackstory :: Character -> Text
secretBackstory = error "secretBackstory is secret."

typeName :: Character -> Text
typeName = either (const "Droid") (const "Human")

luke :: Character
luke = Right luke'

luke' :: Human
luke' = Human
  { _humanChar = CharCommon
      { _id_        = "1000"
      , _name       = "Luke Skywalker"
      , _friends    = ["1002","1003","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

vader :: Human
vader = Human
  { _humanChar = CharCommon
      { _id_        = "1001"
      , _name       = "Darth Vader"
      , _friends    = ["1004"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Tatooine"
  }

han :: Human
han = Human
  { _humanChar = CharCommon
      { _id_        = "1002"
      , _name       = "Han Solo"
      , _friends    = ["1000","1003","2001" ]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = mempty
  }

leia :: Human
leia = Human
  { _humanChar = CharCommon
      { _id_        = "1003"
      , _name       = "Leia Organa"
      , _friends    = ["1000","1002","2000","2001"]
      , _appearsIn  = [4,5,6]
      }
  , homePlanet = "Alderaan"
  }

tarkin :: Human
tarkin = Human
  { _humanChar = CharCommon
    { _id_        = "1004"
    , _name       = "Wilhuff Tarkin"
    , _friends    = ["1001"]
    , _appearsIn  = [4]
    }
  , homePlanet = mempty
  }

threepio :: Droid
threepio = Droid
  { _droidChar = CharCommon
      { _id_ = "2000"
      , _name = "C-3PO"
      , _friends = ["1000","1002","1003","2001" ]
      , _appearsIn = [ 4, 5, 6 ]
      }
  , primaryFunction = "Protocol"
  }

artoo :: Character
artoo = Left artoo'

artoo' :: Droid
artoo' = Droid
  { _droidChar = CharCommon
      { _id_        = "2001"
      , _name       = "R2-D2"
      , _friends    = ["1000","1002","1003"]
      , _appearsIn  = [4,5,6]
      }
  , primaryFunction = "Astrometch"
  }

-- ** Helper functions

getHero :: Int -> Character
getHero 5 = luke
getHero _ = artoo

getHeroIO :: Int -> IO Character
getHeroIO = pure . getHero

getHuman :: ID -> Maybe Character
getHuman = fmap Right . getHuman'

getHuman' :: ID -> Maybe Human
getHuman' "1000" = Just luke'
getHuman' "1001" = Just vader
getHuman' "1002" = Just han
getHuman' "1003" = Just leia
getHuman' "1004" = Just tarkin
getHuman' _      = Nothing

getDroid :: ID -> Maybe Character
getDroid = fmap Left . getDroid'

getDroid' :: ID -> Maybe Droid
getDroid' "2000" = Just threepio
getDroid' "2001" = Just artoo'
getDroid' _      = Nothing

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char

getEpisode :: Int -> Maybe Text
getEpisode 4 = Just "NEWHOPE"
getEpisode 5 = Just "EMPIRE"
getEpisode 6 = Just "JEDI"
getEpisode _ = Nothing
