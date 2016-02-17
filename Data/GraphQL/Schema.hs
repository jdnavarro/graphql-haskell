{-# LANGUAGE CPP #-}
module Data.GraphQL.Schema
  ( Schema(..)
  , QueryRoot
  , ResolverO
  , ResolverM
  , Output(..)
  , Subs
  , Scalar(..)
  , withField
  , withFieldFinal
  , withFields
  , withArgument
  , outputTraverse
  , fields
  -- * Reexports
  , Field
  , Argument
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable (traverse)
#endif
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.Foldable (fold)
import Data.String (IsString(fromString))

import Data.Aeson (ToJSON(toJSON))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text, pack)
import qualified Data.Text as T (null)

import Data.GraphQL.AST

data Schema f = Schema (QueryRoot f)

type QueryRoot f = ResolverM f

-- TODO: Come up with a unique data type or better renaming
type ResolverM f =  Field  -> f (HashMap Text Output)
type ResolverO f = [Field] -> f Output

data Output = OutputObject (HashMap Text Output)
            | OutputList [Output]
            | OutputScalar Scalar
            | OutputEnum Text
              deriving (Show)

type Subs = Text -> Maybe Text

-- TODO: GraphQL spec for Integer Scalar is 32bits
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

-- * Helpers

withField :: Alternative f => Text -> ([Argument] -> ResolverO f) -> ResolverM f
withField n f (Field alias name' args _ sels) =
  if n == name'
     then HashMap.singleton aliasOrName <$> f args (fields sels)
     else empty
  where
    aliasOrName = if T.null alias then name' else alias

withFieldFinal :: Alternative f => Text -> Output -> ResolverM f
withFieldFinal n o fld@(Field _ _ [] _ []) = withField n (\_ _ -> pure o) fld
withFieldFinal _ _ _ = empty

withFields :: Alternative f => ResolverM f -> ResolverO f
withFields f = fmap (OutputObject . fold) . traverse f

outputTraverse :: Applicative f => (a -> f Output) -> [a] -> f Output
outputTraverse f = fmap OutputList . traverse f

withArgument :: Text -> [Argument] -> Maybe Scalar
withArgument x [Argument n s] = if x == n then scalarValue s else Nothing
withArgument _ _ = Nothing

scalarValue :: Value -> Maybe Scalar
scalarValue (ValueInt x) = Just . ScalarInt $ fromIntegral x
scalarValue (ValueString (StringValue x)) = Just $ ScalarString x
scalarValue _ = Nothing

fields :: SelectionSet -> [Field]
fields = catMaybes . fmap field

field :: Selection -> Maybe Field
field (SelectionField x) = Just x
field _ = Nothing
