module Data.GraphQL.Schema where

import Data.Text (Text)
import Data.HashMap.Lazy (HashMap)

data Schema = Schema QueryRoot MutationRoot

type QueryRoot = ObjectOutput

type MutationRoot = ObjectOutput

type ObjectOutput = HashMap Text Output

type ObjectInput =  HashMap Text Input

data Type = TypeScalar Scalar
          | TypeOutputObject ObjectOutput
          | TypeInterface Interface
          | TypeUnion Union
          | TypeEnum Scalar
          | TypeInputObject ObjectInput
          | TypeList List
          | TypeNonNull NonNull

data Output = OutputScalar Scalar
            | OutputObject ObjectOutput
            | OutputInterface Interface
            | OutputUnion Union
            | OutputEnum Scalar
            | OutputList List
            | OutputNonNull NonNull

data Input = InputScalar Scalar
           | InputObject ObjectInput
           | InputEnum Scalar
           | InputList List
           | InputNonNull NonNull

data Scalar = ScalarInt Int
            | ScalarFloat Double
            | ScalarString Text
            | ScalarBool Bool
            | ScalarID Text

newtype Interface = Interface (HashMap Text Output)

newtype Union = Union [ObjectOutput]

type List = [Type]

type NonNull = Type
