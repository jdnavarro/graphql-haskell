{-# LANGUAGE CPP #-}
module Data.GraphQL.Schema where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Data.String (IsString(fromString))

import Data.Aeson (ToJSON(toJSON))
import Data.HashMap.Strict (HashMap)
import Data.Text (Text, pack)

data Schema f = Schema (QueryRoot f) -- (Maybe  MutationRoot)

type QueryRoot f = Resolver f

type Resolver f = Input -> f Output

data Output = OutputObject (HashMap Text Output)
            | OutputList [Output]
            | OutputScalar Scalar
            | OutputEnum Text
              deriving (Show)
           -- | OutputUnion [Output]
           -- | OutputNonNull (Output)

type Argument = (Text, Scalar)

type Subs = Text -> Maybe Scalar

data Input = InputField Text [Argument] [Input]
             deriving (Show)

-- TODO: Make ScalarInt Int32
data Scalar = ScalarInt     Int
            | ScalarFloat   Double
            | ScalarString  Text
            | ScalarBoolean Bool
            | ScalarID      Text
              deriving (Show)

instance IsString Scalar where
  fromString = ScalarString . pack

instance ToJSON Scalar where
    toJSON (ScalarInt     x) = toJSON x
    toJSON (ScalarFloat   x) = toJSON x
    toJSON (ScalarString  x) = toJSON x
    toJSON (ScalarBoolean x) = toJSON x
    toJSON (ScalarID      x) = toJSON x

instance ToJSON Output where
    toJSON (OutputObject x) = toJSON $ toJSON <$> x
    toJSON (OutputList   x) = toJSON $ toJSON <$> x
    toJSON (OutputScalar x) = toJSON x
    toJSON (OutputEnum   x) = toJSON x

