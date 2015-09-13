{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Parser where

import Prelude hiding (takeWhile)
import Control.Applicative (Alternative, (<|>), empty, many)
import Data.Char

import Data.Attoparsec.Text
  ( Parser
  , (<?>)
  , anyChar
  , decimal
  , double
  , endOfLine
  , isEndOfLine
  , many1
  , manyTill
  , satisfy
  , sepBy
  , sepBy1
  , skipMany
  , skipSpace
  , skipWhile
  , signed
  , space
  , takeWhile
  , takeWhile1
  )

import Data.GraphQL.AST

-- * Name

-- XXX: Handle starting `_` and no number at the beginning:
-- https://facebook.github.io/graphql/#sec-Names
name :: Parser Name
name = takeWhile1 isAlphaNum

-- * Document

document :: Parser Document
document = s *>
    (Document <$> many1_ definition
             -- Try SelectionSet when no definition
              <|> Document <$> pure
                           <$> DefinitionOperation
                           <$> Query mempty empty empty
                           <$> selectionSet)
    <*  s
    <?> "document error!"

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <|> DefinitionType      <$> typeDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition =
        op Query "query"
    <|> op Mutation "mutation"
    <?> "operationDefinition error!"
  where
    op f n = f <$ n <* s1 <*> name <* s1
                          <*> (variableDefinitions <* s <|> empty)
                          <*> directives <* s
                          <*> selectionSet

variableDefinitions :: Parser [VariableDefinition]
variableDefinitions = "(" *> s *> many1_ variableDefinition <* s <* ")"

variableDefinition :: Parser VariableDefinition
variableDefinition =
    VariableDefinition <$> variable <* s <*  ":" <* s
                       <*> type_ <* s
                       <*> value

 -- In defense of good taste, I'm taking liberty of not allowing space between
 -- '$' and the 'name' even though that's not in the spec.
variable :: Parser Variable
variable = Variable <$ "$" <*> name

selectionSet :: Parser SelectionSet
selectionSet = "{" *> s *> (SelectionSet <$> many1_ selection) <* s <* "}"

selection :: Parser Selection
selection = SelectionField <$> field
            -- Inline first to catch `on` case
        <|> SelectionInlineFragment <$> inlineFragment
        <|> SelectionFragmentSpread <$> fragmentSpread
        <?> "selection error!"

field :: Parser Field
field = Field <$> (alias <* s1 <|> empty)
              <*> name <* s
              <*> (arguments <* s <|> empty)
              <*> directives <* s
              <*> selectionSet
              <?> "field error!"

alias :: Parser Alias
alias = name <* s <* ":"

arguments :: Parser [Argument]
arguments = "(" *> s *> many1_ argument <* s <* ")"

argument :: Parser Argument
argument = Argument <$> name <* s <* ":" <* s <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
-- TODO: Make sure it fails when `... on`.
-- See https://facebook.github.io/graphql/#FragmentSpread
fragmentSpread = FragmentSpread <$ "..." <* s <*> name <* s1 <*> many_ directive

-- InlineFragment tried first in order to guard against 'on' keyword
inlineFragment :: Parser InlineFragment
inlineFragment = InlineFragment <$ "..." <* s <* "on" <* s1
             <*> typeCondition <* s
             <*> directives <* s
             <*> selectionSet

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition =
    FragmentDefinition <$  "fragment" <* s1
                       <*> name <* s
                       <*  "on" <* s1
                       <*> typeCondition <* s
                       <*> directives <* s
                       <*> selectionSet

typeCondition :: Parser TypeCondition
typeCondition = namedType

-- * Values

-- This will try to pick the first type it can parse. If you are working with
--   explicit types use the `typedValue` parser.
value :: Parser Value
value = -- TODO: Handle arbitrary precision.
        ValueInt     <$> signed decimal
    <|> ValueFloat   <$> signed double
    <|> ValueBoolean <$> bool
    -- TODO: Handle escape characters, unicode, etc
    <|> ValueString <$ "\"" <*> takeWhile isAlphaNum <* "\""
    -- `true` and `false` have been tried before
    <|> ValueEnum <$> name
    <|> ValueList <$> listValue
    <|> ValueObject <$> objectValue

-- Notice it can be empty
listValue :: Parser ListValue
listValue = ListValue <$ "[" <* s <*> many_ value <* s <* "]"

-- Notice it can be empty
objectValue :: Parser ObjectValue
objectValue = ObjectValue <$ "{" <* s <*> many_ objectField <* s <* "}"

objectField :: Parser ObjectField
objectField = ObjectField <$> name <* s <* ":" <* s <*> value

bool :: Parser Bool
bool = True  <$ "true"
   <|> False <$ "false"

-- * Directives

directives :: Parser [Directive]
directives = many_ directive

directive :: Parser Directive
directive = Directive <$ "@" <*> name <* s <*> arguments

-- * Type Reference

type_ :: Parser Type
type_ = TypeNamed   <$> namedType
    <|> TypeList    <$> listType
    <|> TypeNonNull <$> nonNullType

namedType :: Parser NamedType
namedType = NamedType <$> name

listType :: Parser ListType
listType = ListType <$ "[" <* s <*> type_ <* s <* "]"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> namedType <* s <* "!"
          <|> NonNullTypeList  <$> listType  <* s <* "!"

-- * Type Definition

-- TODO: what is Const Variable?
-- https://facebook.github.io/graphql/#Value
typeDefinition :: Parser TypeDefinition
typeDefinition =
        TypeDefinitionObject        <$> objectTypeDefinition
    <|> TypeDefinitionInterface     <$> interfaceTypeDefinition
    <|> TypeDefinitionUnion         <$> unionTypeDefinition
    <|> TypeDefinitionScalar        <$> scalarTypeDefinition
    <|> TypeDefinitionEnum          <$> enumTypeDefinition
    <|> TypeDefinitionInputObject   <$> inputObjectTypeDefinition
    <|> TypeDefinitionTypeExtension <$> typeExtensionDefinition
    <?> "typeDefinition error!"

objectTypeDefinition :: Parser ObjectTypeDefinition
objectTypeDefinition = ObjectTypeDefinition
    <$  "type" <* s1
    <*> name <* s1
    <*> (interfaces <* s <|> empty)
    <*> fieldDefinitions
    <?> "objectTypeDefinition error!"

interfaces :: Parser Interfaces
interfaces = "implements" *> s1 *> many1_ namedType

fieldDefinitions :: Parser [FieldDefinition]
fieldDefinitions = "{" *> s *> many1_ fieldDefinition <* s <* "}"

fieldDefinition :: Parser FieldDefinition
fieldDefinition = FieldDefinition
    <$> name <* s
    <*> (argumentsDefinition <* s <|> empty)
    <*  ":" <* s
    <*> type_

argumentsDefinition :: Parser ArgumentsDefinition
argumentsDefinition = inputValueDefinitions

inputValueDefinitions :: Parser [InputValueDefinition]
inputValueDefinitions = "(" *> s *> many1_ inputValueDefinition <* s <* ")"

inputValueDefinition :: Parser InputValueDefinition
inputValueDefinition = InputValueDefinition
    <$> name <* s
    <*  ":" <* s
    <*> type_ <* s
    <*> (value <|> empty)

interfaceTypeDefinition :: Parser InterfaceTypeDefinition
interfaceTypeDefinition = InterfaceTypeDefinition
    <$  "interface" <* s1
    <*> name <* s
    <*> fieldDefinitions

unionTypeDefinition :: Parser UnionTypeDefinition
unionTypeDefinition = UnionTypeDefinition
    <$  "union" <* s1
    <*> name <* s
    <*> unionMembers
  where
    -- This should take care of standalone `NamedType`
    unionMembers = namedType `sepBy1` (s *> "|" <* s)

scalarTypeDefinition :: Parser ScalarTypeDefinition
scalarTypeDefinition = ScalarTypeDefinition <$  "scalar" <* s1 <*> name

enumTypeDefinition :: Parser EnumTypeDefinition
enumTypeDefinition = EnumTypeDefinition
    <$  "enum" <* s1
    <*> name <* s
    <*> enumValueDefinitions

enumValueDefinitions :: Parser [EnumValueDefinition]
enumValueDefinitions = "{" *> s *> many1_ enumValueDefinition <* s <* "}"

enumValueDefinition :: Parser EnumValueDefinition
enumValueDefinition = EnumValueDefinition <$> name

inputObjectTypeDefinition :: Parser InputObjectTypeDefinition
inputObjectTypeDefinition = InputObjectTypeDefinition
    <$  "input" <* s1
    <*> name <* s
    <*> inputValueDefinitions

typeExtensionDefinition :: Parser TypeExtensionDefinition
typeExtensionDefinition = TypeExtensionDefinition
    <$  "extend" <* s1
    <*> objectTypeDefinition

-- * Internal

many_ :: Parser a -> Parser [a]
many_ = flip sepBy space'

many1_ :: Parser a -> Parser [a]
many1_ = flip sepBy1 space'

space' :: Parser Char
space' = satisfy isSpace'

s :: Parser ()
s = comments <|> skipWhile isSpace'

s1 :: Parser ()
s1 = comments <|> space' *> s

isSpace' :: Char -> Bool
isSpace' c = isSpace c || ',' == c || isEndOfLine c

comments :: Parser ()
comments = skipMany comment

comment :: Parser ()
comment = () <$ "#" <* manyTill anyChar endOfLine
