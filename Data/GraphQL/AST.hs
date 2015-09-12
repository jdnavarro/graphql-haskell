module Data.GraphQL.AST where

import Data.Text (Text)

-- * Name

type Name = Text

-- * Document

newtype Document = Document [Definition] deriving (Eq,Show)

data Definition = DefinitionOperation OperationDefinition
                | DefinitionFragment  FragmentDefinition
                | DefinitionType      TypeDefinition
                  deriving (Eq,Show)

data OperationDefinition =
    Query        (Maybe [VariableDefinition]) (Maybe [Directive]) SelectionSet
  | Mutation     (Maybe [VariableDefinition]) (Maybe [Directive]) SelectionSet
  | Subscription (Maybe [VariableDefinition]) (Maybe [Directive]) SelectionSet
    deriving (Eq,Show)

data VariableDefinition = VariableDefinition Variable Type (Maybe DefaultValue)
                          deriving (Eq,Show)

newtype Variable = Variable Name deriving (Eq,Show)

newtype SelectionSet = SelectionSet [Selection] deriving (Eq,Show)

data Selection = SelectionField Field
               | SelectionFragmentSpread FragmentSpread
               | SelectionInlineFragment InlineFragment
                 deriving (Eq,Show)

data Field = Field (Maybe Alias) Name (Maybe [Argument])
                                      (Maybe [Directive])
                                      (Maybe SelectionSet)
             deriving (Eq,Show)

type Alias = Name

data Argument = Argument Name Value deriving (Eq,Show)

-- * Fragments

data FragmentSpread = FragmentSpread Name (Maybe [Directive])
                      deriving (Eq,Show)

data InlineFragment =
    InlineFragment TypeCondition (Maybe [Directive]) SelectionSet
    deriving (Eq,Show)

data FragmentDefinition =
    FragmentDefinition Name TypeCondition (Maybe [Directive]) SelectionSet
    deriving (Eq,Show)

type TypeCondition = NamedType

-- * Values

data Value = ValueVariable Variable
           | ValueInt Int
           | ValueFloat Float
           | ValueString Text
           | ValueBoolean Bool
           | ValueEnum Name
           | ValueList ListValue
           | ValueObject ObjectValue
             deriving (Eq,Show)

newtype ListValue = ListValue [Value] deriving (Eq,Show)

newtype ObjectValue = ObjectValue [ObjectField] deriving (Eq,Show)

data ObjectField = ObjectField Name Value deriving (Eq,Show)

type DefaultValue = Value

-- * Directives

data Directive = Directive Name (Maybe [Argument]) deriving (Eq,Show)

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

data ObjectTypeDefinition = ObjectTypeDefinition Name (Maybe Interfaces) [FieldDefinition]
                            deriving (Eq,Show)

type Interfaces = [NamedType]

data FieldDefinition = FieldDefinition Name [InputValueDefinition]
                       deriving (Eq,Show)

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
