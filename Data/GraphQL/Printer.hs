{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Printer where

import Prelude hiding (unwords)
import Data.Monoid ((<>))

import Data.Text (Text, intercalate, pack, unwords)

import Data.GraphQL.AST

-- Uniplate could avoid boilerplate, but is it worth bringing the
-- extra dependency?

-- * Document

document :: Document -> Text
document (Document defs) = intercalate "\n\n" $ definition <$> defs

definition :: Definition -> Text
definition (DefinitionOperation x) = operationDefinition x
definition (DefinitionFragment  x) = fragmentDefinition x
definition (DefinitionType      x) = typeDefinition x

operationDefinition :: OperationDefinition -> Text
operationDefinition (Query    n) = "query "    <> node n
operationDefinition (Mutation n) = "mutation " <> node n

node :: Node -> Text
node (Node name vds ds ss) =
       name
    <> optempty variableDefinitions vds
    <> optempty (("\SP" <>) . directives) ds
    <> "\SP"
    <> selectionSet ss

variableDefinitions :: [VariableDefinition] -> Text
variableDefinitions = parens . intercalate ", " . fmap variableDefinition

variableDefinition :: VariableDefinition -> Text
variableDefinition (VariableDefinition var ty dv) =
    variable var <> ": " <> type_ ty <> maybe mempty defaultValue dv

defaultValue :: DefaultValue -> Text
defaultValue val = "= " <> value val

variable :: Variable -> Text
variable (Variable name) = "$" <> name

selectionSet :: SelectionSet -> Text
selectionSet = block . fmap selection

selection :: Selection -> Text
selection (SelectionField          x) = field x
selection (SelectionInlineFragment x) = inlineFragment x
selection (SelectionFragmentSpread x) = fragmentSpread x

field :: Field -> Text
field (Field alias name args ds ss) =
       optempty (<> ": ") alias
    <> name
    <> optempty arguments args
    <> optempty (("\SP" <>) . directives) ds
    <> optempty (("\SP" <>) . selectionSet) ss

arguments :: [Argument] -> Text
arguments = parens . intercalate ", " . fmap argument

argument :: Argument -> Text
argument (Argument name v) = name <> ": " <> value v

-- * Fragments

fragmentSpread :: FragmentSpread -> Text
fragmentSpread (FragmentSpread name ds) =
    "..." <> name <> optempty (spaces . directives) ds

inlineFragment :: InlineFragment -> Text
inlineFragment (InlineFragment (NamedType tc) ds ss) =
    "... on" <> tc
             <> optempty (("\SP" <>) . directives) ds
             <> optempty (("\SP" <>) . selectionSet) ss

fragmentDefinition :: FragmentDefinition -> Text
fragmentDefinition (FragmentDefinition name (NamedType tc) ds ss) =
    "fragment " <> name <> " on " <> tc
                <> optempty (("\SP" <>) . directives) ds
                <> (("\SP" <>) . selectionSet) ss

-- * Values

value :: Value -> Text
value (ValueVariable x) = variable x
-- TODO: This will be replaced with `decimal` Buidler
value (ValueInt      x) = pack $ show x
-- TODO: This will be replaced with `decimal` Buidler
value (ValueFloat    x) = pack $ show x
value (ValueBoolean  x) = booleanValue x
value (ValueString   x) = stringValue x
value (ValueEnum     x) = x
value (ValueList     x) = listValue x
value (ValueObject   x) = objectValue x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

-- TODO: Escape characters
stringValue :: StringValue -> Text
stringValue (StringValue x) = x

listValue :: ListValue -> Text
listValue (ListValue vs) = brackets . intercalate ", " $ value <$> vs

objectValue :: ObjectValue -> Text
objectValue (ObjectValue ofs) = block $ objectField <$> ofs

objectField :: ObjectField -> Text
objectField (ObjectField name v) = name <> ": " <> value v

-- * Directives 

directives :: [Directive] -> Text
directives = withSpaces directive

directive :: Directive -> Text
directive (Directive name args) = "@" <> name <> optempty arguments args

-- * Type Reference

type_ :: Type -> Text
type_ (TypeNamed (NamedType x)) = x
type_ (TypeList x) = listType x
type_ (TypeNonNull x) = nonNullType x

namedType :: NamedType -> Text
namedType (NamedType name) = name

listType :: ListType -> Text
listType (ListType ty) = "[" <> type_ ty <> "]"

nonNullType :: NonNullType -> Text
nonNullType (NonNullTypeNamed (NamedType x)) = x <> "!"
nonNullType (NonNullTypeList  x) = listType x <> "!"

typeDefinition :: TypeDefinition -> Text
typeDefinition (TypeDefinitionObject        x) = objectTypeDefinition x
typeDefinition (TypeDefinitionInterface     x) = interfaceTypeDefinition x
typeDefinition (TypeDefinitionUnion         x) = unionTypeDefinition x
typeDefinition (TypeDefinitionScalar        x) = scalarTypeDefinition x
typeDefinition (TypeDefinitionEnum          x) = enumTypeDefinition x
typeDefinition (TypeDefinitionInputObject   x) = inputObjectTypeDefinition x
typeDefinition (TypeDefinitionTypeExtension x) = typeExtensionDefinition x

objectTypeDefinition :: ObjectTypeDefinition -> Text
objectTypeDefinition (ObjectTypeDefinition name ifaces fds) =
    "type " <> name
            <> optempty (("\SP" <>) . interfaces) ifaces
            <> optempty (("\SP" <>) . fieldDefinitions) fds

interfaces :: Interfaces -> Text
interfaces = ("implements " <>) . unwords . fmap namedType

fieldDefinitions :: [FieldDefinition] -> Text
fieldDefinitions = block . fmap fieldDefinition

fieldDefinition :: FieldDefinition -> Text
fieldDefinition (FieldDefinition name args ty) =
    name <> optempty argumentsDefinition args
         <> ": "
         <> type_ ty

argumentsDefinition :: ArgumentsDefinition -> Text
argumentsDefinition = parens . intercalate ", " . fmap inputValueDefinition

inputValueDefinitions :: [InputValueDefinition] -> Text
inputValueDefinitions = block . fmap inputValueDefinition

inputValueDefinition :: InputValueDefinition -> Text
inputValueDefinition (InputValueDefinition name ty dv) =
    name <> ": " <> type_ ty <> maybe mempty (("\SP" <>) . defaultValue) dv

interfaceTypeDefinition :: InterfaceTypeDefinition -> Text
interfaceTypeDefinition (InterfaceTypeDefinition name fds) =
    "interface " <> name <> "\SP" <> fieldDefinitions fds

unionTypeDefinition :: UnionTypeDefinition -> Text
unionTypeDefinition (UnionTypeDefinition name ums) =
    "union " <> name <> " = " <> unionMembers ums

unionMembers :: [NamedType] -> Text
unionMembers = intercalate " | " . fmap namedType

scalarTypeDefinition :: ScalarTypeDefinition -> Text
scalarTypeDefinition (ScalarTypeDefinition name) = "scalar " <> name

enumTypeDefinition :: EnumTypeDefinition -> Text
enumTypeDefinition (EnumTypeDefinition name evds) =
    "enum " <> name
            <> block (enumValueDefinition <$> evds)

enumValueDefinition :: EnumValueDefinition -> Text
enumValueDefinition (EnumValueDefinition name) = name

inputObjectTypeDefinition :: InputObjectTypeDefinition -> Text
inputObjectTypeDefinition (InputObjectTypeDefinition name ivds) =
    "input " <> name <> "\SP" <> inputValueDefinitions ivds

typeExtensionDefinition :: TypeExtensionDefinition -> Text
typeExtensionDefinition (TypeExtensionDefinition otd) =
    "extend " <> objectTypeDefinition otd

-- * Internal

spaces :: Text -> Text
spaces txt = "\SP" <> txt <> "\SP"

parens :: Text -> Text
parens txt = "(" <> txt <> ")"

brackets :: Text -> Text
brackets txt = "[" <> txt <> "]"

withSpaces :: (a -> Text) -> [a] -> Text
withSpaces f = intercalate "\SP" . fmap f

withCommas :: (a -> Text) -> [a] -> Text
withCommas f = intercalate ", " . fmap f

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs

block :: [Text] -> Text
block txts = "{\n" <> intercalate "  \n" txts <> "\n}"
