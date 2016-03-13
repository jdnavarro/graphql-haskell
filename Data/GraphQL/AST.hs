{- | This module defines an
     abstract syntax tree for the GraphQL language, based on
     <https://facebook.github.io/graphql/ Facebook's GraphQL Specification>.
-}

module Data.GraphQL.AST where

import Data.Int (Int32)
import Data.String (IsString(fromString))
import Data.Text (Text, pack)

-- * Name

type Name = Text

-- * Document

newtype Document = Document [Definition] deriving (Eq,Show)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                | DefinitionType      TypeDefinition
                  deriving (Eq,Show)

data OperationDefinition = Query    Node
                         | Mutation Node
                           deriving (Eq,Show)

data Node = Node Name [VariableDefinition] [Directive] SelectionSet
            deriving (Eq,Show)

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq,Show)

instance IsString Variable where
    fromString = Variable . pack

type SelectionSet = [Selection]

data Selection = SelectionField Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
                 deriving (Eq,Show)

{- | <https://facebook.github.io/graphql/#sec-Language.Query-Document.Fields Field Specification>

     A selection set is primarily composed of fields.
     A field describes one discrete piece of information
     available to request within a selection set.

     Some fields describe complex data or relationships to other data.
     In order to further explore this data, a field may itself contain
     a selection set, allowing for deeply nested requests.
     All GraphQL operations must specify their selections down to
     fields which return scalar values to ensure an unambiguously
     shaped response.

-}
data Field = Field Alias Name [Argument] [Directive] SelectionSet
             deriving (Eq,Show)

type Alias = Name

{- | <https://facebook.github.io/graphql/#sec-Language.Query-Document.Arguments Argument Specification>

     Fields are conceptually functions which return values,
     and occasionally accept arguments which alter their behavior.
     These arguments often map directly to function arguments within a
     GraphQL server’s implementation.

-}
data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name [Directive]
                      deriving (Eq,Show)

data InlineFragment =
    InlineFragment TypeCondition [Directive] SelectionSet
    deriving (Eq,Show)

data FragmentDefinition =
    FragmentDefinition Name TypeCondition [Directive] SelectionSet
    deriving (Eq,Show)

type TypeCondition = NamedType

-- * Values

{- | <https://facebook.github.io/graphql/#sec-Input-Values Input Value Specification>

    Field and directive arguments accept input values
    of various literal primitives; input values can be scalars,
    enumeration values, lists, or input objects.

    If not defined as constant (for example, in DefaultValue),
    input values can be specified as a variable.
    List and inputs objects may also contain
    variables (unless defined to be constant).

-}
data Value = ValueVariable Variable
           | ValueInt Int32
           -- GraphQL Float is double precison
           | ValueFloat Double
           | ValueBoolean Bool
           | ValueString Text
           | ValueEnum Name
           | ValueList ListValue
           | ValueObject ObjectValue
             deriving (Eq,Show)

newtype ListValue = ListValue [Value] deriving (Eq,Show)

newtype ObjectValue = ObjectValue [ObjectField] deriving (Eq,Show)

data ObjectField = ObjectField Name Value deriving (Eq,Show)

type DefaultValue = Value

-- * Directives

data Directive = Directive Name [Argument] deriving (Eq,Show)

-- * Type Reference

data Type = TypeNamed NamedType
          | TypeList ListType
          | TypeNonNull NonNullType
            deriving (Eq,Show)

newtype NamedType = NamedType Name deriving (Eq,Show)

newtype ListType = ListType Type deriving (Eq,Show)

data NonNullType = NonNullTypeNamed NamedType
                 | NonNullTypeList  ListType
                   deriving (Eq,Show)

-- * Type definition

data TypeDefinition = TypeDefinitionObject        ObjectTypeDefinition
                    | TypeDefinitionInterface     InterfaceTypeDefinition
                    | TypeDefinitionUnion         UnionTypeDefinition
                    | TypeDefinitionScalar        ScalarTypeDefinition
                    | TypeDefinitionEnum          EnumTypeDefinition
                    | TypeDefinitionInputObject   InputObjectTypeDefinition
                    | TypeDefinitionTypeExtension TypeExtensionDefinition
                      deriving (Eq,Show)

data ObjectTypeDefinition = ObjectTypeDefinition Name Interfaces [FieldDefinition]
                            deriving (Eq,Show)

type Interfaces = [NamedType]

data FieldDefinition = FieldDefinition Name ArgumentsDefinition Type
                       deriving (Eq,Show)

type ArgumentsDefinition = [InputValueDefinition]

data InputValueDefinition = InputValueDefinition Name Type (Maybe DefaultValue)
                            deriving (Eq,Show)

data InterfaceTypeDefinition = InterfaceTypeDefinition Name [FieldDefinition]
                               deriving (Eq,Show)

data UnionTypeDefinition = UnionTypeDefinition Name [NamedType]
                           deriving (Eq,Show)

data ScalarTypeDefinition = ScalarTypeDefinition Name
                            deriving (Eq,Show)

data EnumTypeDefinition = EnumTypeDefinition Name [EnumValueDefinition]
                          deriving (Eq,Show)

newtype EnumValueDefinition = EnumValueDefinition Name
                              deriving (Eq,Show)

data InputObjectTypeDefinition = InputObjectTypeDefinition Name [InputValueDefinition]
                                 deriving (Eq,Show)

newtype TypeExtensionDefinition = TypeExtensionDefinition ObjectTypeDefinition
                                  deriving (Eq,Show)
