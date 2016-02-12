{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars.QueryTests where

import Data.Aeson (object, (.=))
import Data.Text (Text)
import Text.RawString.QQ (r)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

import Data.GraphQL

import Test.StarWars.Schema

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

