module Data.GraphQL.Schema where

import Data.Maybe (catMaybes)

import Data.Text (Text)
import Data.Aeson (ToJSON(toJSON))

data Schema f = Schema (QueryRoot f) -- (Maybe  MutationRoot)

type QueryRoot f = Resolver f

type Resolver f = Input -> f (Output f)

data Output f = OutputResolver (Resolver f)
              | OutputList (f [Output f])
              | OutputScalar (f Scalar)
           -- | OutputUnion [Output]
           -- | OutputEnum [Scalar]
           -- | OutputNonNull (Output)

data Input = InputScalar Scalar
           | InputField Text
           | InputList [Input]
             deriving (Show)

field :: Input -> Maybe Text
field (InputField x) = Just x
field _ = Nothing

fields :: [Input] -> [Text]
fields = catMaybes . fmap field

data Scalar = ScalarInt     Int
            | ScalarFloat   Double
            | ScalarString  Text
            | ScalarBoolean Bool
            | ScalarID      Text
              deriving (Show)

instance ToJSON Scalar where
    toJSON (ScalarInt     x) = toJSON x
    toJSON (ScalarFloat   x) = toJSON x
    toJSON (ScalarString  x) = toJSON x
    toJSON (ScalarBoolean x) = toJSON x
    toJSON (ScalarID      x) = toJSON x
