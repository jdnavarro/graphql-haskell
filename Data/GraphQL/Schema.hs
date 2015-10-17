module Data.GraphQL.Schema where

import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)

data Schema f = Schema (QueryRoot f) (Maybe (MutationRoot f))

type QueryRoot f = Object f

type MutationRoot f = Object f

type Object f = HashMap Text (Input -> f Output)

type ObjectInput =  HashMap Text Input

data Output = OutputScalar Scalar
            | OutputObject (HashMap Text Output)
            | OutputUnion [Output]
            | OutputEnum Scalar
            | OutputList [Output]
            | OutputNonNull Output
            | InputError

data Input = InputScalar Scalar
           | InputObject ObjectInput
           | InputEnum Scalar
           | InputList [Output]
           | InputNonNull Input

data Scalar = ScalarInt Int
            | ScalarFloat Double
            | ScalarString Text
            | ScalarBool Bool
            | ScalarID Text

newtype Interface f = Interface (Object f)
