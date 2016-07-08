{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Test.StarWars.QueryTests (test) where

import qualified Data.Aeson as Aeson (Value(Null), toJSON)
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
      $ object [ "data" .= object ["hero" .= object ["id" .= ("2001" :: Text)]]]
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
      $ object [ "data" .= object [
          "hero" .= object [
              "id" .= ("2001" :: Text)
            , "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  object [lukeName]
                , object [hanName]
                , object [leiaName]
                ]
            ]
        ]]
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
      $ object [ "data" .= object [
          "hero" .= object [
              "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  object [
                      "name" .= ("Luke Skywalker" :: Text)
                    , "appearsIn" .= ["NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [hanName]
                        , object [leiaName]
                        , object ["name" .= ("C-3PO" :: Text)]
                        , object ["name" .= ("R2-D2" :: Text)]
                        ]
                    ]
                , object [
                      hanName
                    , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [lukeName]
                        , object [leiaName]
                        , object ["name" .= ("R2-D2" :: Text)]
                        ]
                    ]
                , object [
                      leiaName
                    , "appearsIn" .= [ "NEWHOPE","EMPIRE","JEDI" :: Text]
                    , "friends" .= [
                          object [lukeName]
                        , object [hanName]
                        , object ["name" .= ("C-3PO" :: Text)]
                        , object ["name" .= ("R2-D2" :: Text)]
                        ]
                    ]
                ]
            ]
        ]]
    , testCase "Luke ID" . testQuery
        [r| query FetchLukeQuery {
              human(id: "1000") {
                name
              }
            }
        |]
      $ object [ "data" .= object [
          "human" .= object [lukeName]
        ]
    ]]
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
      $ object [ "data" .= object [
          "human" .= object [lukeName]
        ]]
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
      $ object [ "data" .= object [
          "human" .= object [hanName]
        ]]
    , testCase "Invalid ID" . testQueryParams
        (\v -> if v == "id"
                  then Just "Not a valid ID"
                  else Nothing)
        [r| query humanQuery($id: String!) {
              human(id: $id) {
                name
              }
            }
        |] $ object ["data" .= object ["human" .= object ["name" .= Aeson.Null]],
                     "errors" .= Aeson.toJSON [object ["message" .= ("field name not resolved." :: Text)]]]
        -- TODO: This test is directly ported from `graphql-js`, however do we want
        -- to mimic the same behavior? Is this part of the spec? Once proper
        -- exceptions are implemented this test might no longer be meaningful.
        -- If the same behavior needs to be replicated, should it be implemented
        -- when defining the `Schema` or when executing?
        -- $ object [ "data" .= object ["human" .= Aeson.Null] ]
    , testCase "Luke aliased" . testQuery
        [r| query FetchLukeAliased {
              luke: human(id: "1000") {
                name
              }
            }
        |]
      $ object [ "data" .= object [
         "luke" .= object [lukeName]
        ]]
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
      $ object [ "data" .= object [
          "hero" .= object [
              "id" .= ("2001" :: Text)
            , "name" .= ("R2-D2" :: Text)
            , "friends" .= [
                  object ["friendName" .= ("Luke Skywalker" :: Text)]
                , object ["friendName" .= ("Han Solo" :: Text)]
                , object ["friendName" .= ("Leia Organa" :: Text)]
                ]
            ]
        ]]
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
      $ object [ "data" .= object [
          "luke" .= object [lukeName]
        , "leia" .= object [leiaName]
        ]]
  , testGroup "Fragments for complex queries"
    [  testCase "Aliases to query for duplicate content" . testQuery
        [r| query DuplicateFields {
              luke: human(id: "1000") {
                name
                homePlanet
              }
              leia: human(id: "1003") {
                name
                homePlanet
              }
            }
        |]
      $ object [ "data" .= object [
          "luke" .= object [lukeName, tatooine]
        , "leia" .= object [leiaName, alderaan]
        ]]
    ,  testCase "Fragment for duplicate content" . testQuery
        [r|  query UseFragment {
              luke: human(id: "1000") {
                ...HumanFragment
              }
              leia: human(id: "1003") {
                ...HumanFragment
              }
            }
            fragment HumanFragment on Human {
              name
              homePlanet
            }
        |]
      $ object [ "data" .= object [
          "luke" .= object [lukeName, tatooine]
        , "leia" .= object [leiaName, alderaan]
        ]]
    ]
  ]
  where
    lukeName = "name" .= ("Luke Skywalker" :: Text)
    leiaName = "name" .= ("Leia Organa" :: Text)
    hanName = "name" .= ("Han Solo" :: Text)
    tatooine = "homePlanet" .= ("Tatooine" :: Text)
    alderaan = "homePlanet" .= ("Alderaan" :: Text)

testQuery :: Text -> Aeson.Value -> Assertion
testQuery q expected = graphql schema q @?= Just expected

-- testFail :: Text -> Assertion
-- testFail q = graphql schema q @?= Nothing

testQueryParams :: Subs -> Text -> Aeson.Value -> Assertion
testQueryParams f q expected = graphqlSubs schema f q @?= Just expected

-- testFailParams :: Subs -> Text -> Assertion
-- testFailParams f q = graphqlSubs schema f q @?= Nothing
