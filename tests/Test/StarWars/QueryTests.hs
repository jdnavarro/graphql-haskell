{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars.QueryTests (test) where

import qualified Data.Aeson as Aeson (Value)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Text.RawString.QQ (r)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

import Data.GraphQL
import Data.GraphQL.Schema (Subs)

import Test.StarWars.Schema

-- * Test
-- See https://github.com/graphql/graphql-js/blob/master/src/__tests__/starWarsQueryTests.js

test :: TestTree
test = testGroup "Star Wars Query Tests"
  [ testGroup "Basic Queries"
    [ testCase "R2-D2 hero" . testQuery
        [r| query HeroNameQuery {
              hero {
                id
              }
            }
        |]
      $ object ["hero" .= object ["id" .= ("2001" :: Text)]]
    , testCase "R2-D2 ID and friends" . testQuery
        [r| query HeroNameAndFriendsQuery {
              hero {
                id
                name
                friends {
                  name
                }
              }
            }
        |]
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
    [ testCase "R2-D2 friends" . testQuery
        [r| query NestedQuery {
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
            }
        |]
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
    , testCase "Luke ID" . testQuery
        [r| query FetchLukeQuery {
              human(id: "1000") {
                name
              }
            }
        |]
      $ object [
          "human" .= object [
             "name" .= ("Luke Skywalker" :: Text)
          ]
        ]
    ]
    , testCase "Luke ID with variable" . testQueryParams
         (\v -> if v == "someId"
                   then Just "1000"
                   else Nothing)
         [r| query FetchSomeIDQuery($someId: String!) {
               human(id: $someId) {
                 name
               }
             }
         |]
      $ object [
          "human" .= object ["name" .= ("Luke Skywalker" :: Text)]
        ]
    , testCase "Han ID with variable" . testQueryParams
        (\v -> if v == "someId"
                  then Just "1002"
                  else Nothing)
        [r| query FetchSomeIDQuery($someId: String!) {
              human(id: $someId) {
                name
              }
            }
        |]
      $ object [
          "human" .= object ["name" .= ("Han Solo" :: Text)]
        ]
    , testCase "Invalid ID" $ testFailParams
        (\v -> if v == "id"
                  then Just "Not a valid ID"
                  else Nothing)
        [r| query humanQuery($id: String!) {
              human(id: $id) {
                name
              }
            }
        |]
        -- TODO: This test is directly ported from `graphql-js`, however do we want
        -- to mimic the same behavior? Is this part of the spec? Once proper
        -- exceptions are implemented this test might no longer be meaningful.
        -- If the same behavior needs to be replicated, should it be implemented
        -- when defining the `Schema` or when executing?
        -- $ object ["human" .= Aeson.Null]
    , testCase "Luke aliased" . testQuery
        [r| query FetchLukeAliased {
              luke: human(id: "1000") {
                name
              }
            }
        |]
      $ object [
         "luke" .= object [
           "name" .= ("Luke Skywalker" :: Text)
           ]
        ]
    , testCase "R2-D2 ID and friends aliased" . testQuery
        [r| query HeroNameAndFriendsQuery {
              hero {
                id
                name
                friends {
                  friendName: name
                }
              }
            }
        |]
      $ object [
          "hero" .= object [
              "id" .= ("2001" :: Text)
            , "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  object ["friendName" .= ("Luke Skywalker" :: Text)]
                , object ["friendName" .= ("Han Solo" :: Text)]
                , object ["friendName" .= ("Leia Organa" :: Text)]
                ]
            ]
        ]
    , testCase "Luke and Leia aliased" . testQuery
        [r| query FetchLukeAndLeiaAliased {
              luke: human(id: "1000") {
                name
              }
              leia: human(id: "1003") {
                name
              }
            }
        |]
      $ object [
          "luke" .= object [
            "name" .= ("Luke Skywalker" :: Text)
           ]
        , "leia" .= object [
            "name" .= ("Leia Organa" :: Text)
           ]
        ]
  ]

testQuery :: Text -> Aeson.Value -> Assertion
testQuery q expected = graphql schema q @?= Just expected

-- testFail :: Text -> Assertion
-- testFail q = graphql schema q @?= Nothing

testQueryParams :: Subs -> Text -> Aeson.Value -> Assertion
testQueryParams f q expected = graphqlSubs schema f q @?= Just expected

testFailParams :: Subs -> Text -> Assertion
testFailParams f q = graphqlSubs schema f q @?= Nothing
