-- | This module defines an abstract syntax tree for the @GraphQL@ language based on
--   <https://facebook.github.io/graphql/ Facebook's GraphQL Specification>.
--
-- Target AST for Parser.

module Data.GraphQL.AST where

import Data.Int (Int32)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)

-- * Name

type Name = Text

-- * Document

type Document = NonEmpty Definition

-- * Operations

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                  deriving (Eq,Show)

data OperationDefinition = OperationSelectionSet SelectionSet
                         | OperationDefinition   OperationType
                                                 (Maybe Name)
                                                 VariableDefinitions
                                                 Directives
                                                 SelectionSet
                           deriving (Eq,Show)

data OperationType = Query | Mutation deriving (Eq,Show)

-- * SelectionSet

type SelectionSet = NonEmpty Selection

type SelectionSetOpt = [Selection]

data Selection = SelectionField          Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
                 deriving (Eq,Show)

-- * Field

data Field = Field (Maybe Alias) Name Arguments Directives SelectionSetOpt
             deriving (Eq,Show)

type Alias = Name

-- * Arguments

type Arguments = [Argument]

data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name Directives deriving (Eq,Show)

data InlineFragment = InlineFragment (Maybe TypeCondition) Directives SelectionSet
                      deriving (Eq,Show)

data FragmentDefinition =
  FragmentDefinition FragmentName TypeCondition Directives SelectionSet
  deriving (Eq,Show)

type FragmentName = Name

type TypeCondition = Name

-- Input Values

data Value = ValueVariable Variable
           | ValueInt IntValue
           | ValueFloat FloatValue
           | ValueString StringValue
           | ValueBoolean BooleanValue
           | ValueNull
           | ValueEnum EnumValue
           | ValueList ListValue
           | ValueObject ObjectValue
             deriving (Eq,Show)

type IntValue = Int32

-- GraphQL Float is double precison
type FloatValue = Double

type StringValue = Text

type BooleanValue = Bool

type EnumValue = Name

type ListValue = [Value]

type ObjectValue = [ObjectField]

data ObjectField = ObjectField Name Value deriving (Eq,Show)

-- * Variables

type VariableDefinitions = [VariableDefinition]

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

type Variable = Name

type DefaultValue = Value

-- * Input Types

data Type = TypeNamed   Name
          | TypeList    Type
          | TypeNonNull NonNullType
            deriving (Eq,Show)

data NonNullType = NonNullTypeNamed Name
                 | NonNullTypeList  Type
                   deriving (Eq,Show)

-- * Directives

type Directives = [Directive]

data Directive = Directive Name [Argument] deriving (Eq,Show)
