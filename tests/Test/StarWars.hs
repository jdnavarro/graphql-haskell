{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
import Data.Monoid (mempty)
import Data.Traversable (traverse)
#endif
import Control.Applicative (Alternative, (<|>), empty, liftA2)
import Data.Foldable (fold)
import Data.Maybe (catMaybes)

import Data.Aeson (object, (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import Text.RawString.QQ (r)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL
import Data.GraphQL.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testGroup "Star Wars Query Tests"
  [ testGroup "Basic Queries"
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
  , testGroup "Nested Queries"
    [ testCase "R2-D2 friends" $ (@?=) (graphql schema [r|
  query NestedQuery {
    hero {
      name
      friends {
        name
        appearsIn
        friends {
          name
        }
      }
    }
  }|]) . Just
       $ object [
           "hero" .= object [
                "name" .= ("R2-D2" :: Text)
              , "friends" .= [
                     object [
                         "name" .= ("Luke Skywalker" :: Text)
                       , "appearsIn" .= ["NEWHOPE","EMPIRE","JEDI" :: Text]
                       , "friends" .= [
                             object ["name" .= ("Han Solo" :: Text)]
                           , object ["name" .= ("Leia Organa" :: Text)]
                           , object ["name" .= ("C-3PO" :: Text)]
                           , object ["name" .= ("R2-D2" :: Text)]
                           ]
                       ]
                   , object [
                         "name" .= ("Han Solo" :: Text)
                       , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                       , "friends" .= [
                             object ["name" .= ("Luke Skywalker" :: Text)]
                           , object ["name" .= ("Leia Organa" :: Text)]
                           , object ["name" .= ("R2-D2" :: Text)]
                           ]
                       ]
                   , object [
                         "name" .= ("Leia Organa" :: Text)
                       , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                       , "friends" .= [
                             object ["name" .= ("Luke Skywalker" :: Text)]
                           , object ["name" .= ("Han Solo" :: Text)]
                           , object ["name" .= ("C-3PO" :: Text)]
                           , object ["name" .= ("R2-D2" :: Text)]
                           ]
                       ]
                   ]
               ]
           ]
    , testCase "Luke ID" $ (@?=) (graphql schema [r|
query FetchLukeQuery {
  human(id: "1000") {
    name
  }
}|]) . Just
     $ object [
        "human" .= object [
           "name" .= ("Luke Skywalker" :: Text)
           ]
        ]
    ]
  ]

-- * Schema
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsSchema.js

type ID = Text

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

-- * Data
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsData.js

-- ** Characters

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

-- I don't think this is cumbersome enough to make it worth using lens.

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
  , homePlanet = "Tatoonie"
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


getHuman :: Alternative f => ID -> f Character
getHuman = fmap Right . getHuman'

getHuman' :: Alternative f => ID -> f Human
getHuman' "1000" = pure luke'
getHuman' "1001" = pure vader
getHuman' "1002" = pure han
getHuman' "1003" = pure leia
getHuman' "1004" = pure tarkin
getHuman' _      = empty

getDroid :: Alternative f => ID -> f Character
getDroid = fmap Left . getDroid'

getDroid' :: Alternative f => ID -> f Droid
getDroid' "2000" = pure threepio
getDroid' "2001" = pure artoo'
getDroid' _      = empty

getFriends :: Character -> [Character]
getFriends char = catMaybes $ liftA2 (<|>) getDroid getHuman <$> friends char
