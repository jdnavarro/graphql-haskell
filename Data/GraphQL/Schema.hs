module Data.GraphQL.Schema where

import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)

data Schema f = Schema (QueryRoot f) (Maybe (MutationRoot f))

type QueryRoot f = Map f

type MutationRoot f = Map f

type Map f = HashMap Text (Resolver f)

type Resolver f = Input -> Output f

data Output f = OutputScalar (f Scalar)
              | OutputMap (Map f)
              | OutputUnion [Map f]
              | OutputEnum (f Scalar)
              | OutputList [Output f]
              | OutputNonNull (Output f)
              | InputError

data Input = InputScalar Scalar
           | InputEnum Scalar
           | InputList [Input]
           | InputNonNull Input

data Scalar = ScalarInt Int
            | ScalarFloat Double
            | ScalarString Text
            | ScalarBool Bool
            | ScalarID Text
