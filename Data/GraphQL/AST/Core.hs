-- | This is the AST meant to be executed.
module Data.GraphQL.AST.Core where

import Data.List.NonEmpty (NonEmpty)

import Data.Text (Text)

newtype Name = Name Text deriving (Eq,Show)

newtype Document = Document (NonEmpty Operation) deriving (Eq,Show)

data Operation = Query (NonEmpty Field)
               | Mutation (NonEmpty Field)
                 deriving (Eq,Show)

data Field = Field Name [Argument] [Field] deriving (Eq,Show)

data Argument = Argument Name Value deriving (Eq,Show)

data Value = ValueInt Int32
           -- GraphQL Float is double precision
           | ValueFloat Double
           | ValueBoolean Bool
           | ValueString Text
           | ValueEnum Name
           | ValueList [Value]
           | ValueObject [ObjectField]
             deriving (Eq,Show)

data ObjectField = ObjectField Name Value deriving (Eq,Show)
