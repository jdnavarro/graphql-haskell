{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- | This module defines a parser for @GraphQL@ request documents.
module Data.GraphQL.Parser where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>), Alternative, empty, many, optional)
import Control.Monad (when)
import Data.Char (isDigit, isSpace)
import Data.Foldable (traverse_)
import Data.Monoid ((<>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Scientific (floatingOrInteger)

import Data.Text (Text, append)
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text
  ( Parser
  , (<?>)
  , anyChar
  , endOfLine
  , inClass
  , many1
  , manyTill
  , option
  , peekChar
  , scientific
  , takeWhile
  , takeWhile1
  )

import Data.GraphQL.AST

-- * Name

name :: Parser Name
name = tok $ append <$> takeWhile1 isA_z
                    <*> takeWhile ((||) <$> isDigit <*> isA_z)
  where
    -- `isAlpha` handles many more Unicode Chars
    isA_z =  inClass $ '_' : ['A'..'Z'] <> ['a'..'z']

-- * Document

document :: Parser Document
document = whiteSpace *> manyNE definition

definition :: Parser Definition
definition = DefinitionOperation <$> operationDefinition
         <|> DefinitionFragment  <$> fragmentDefinition
         <?> "definition error!"

operationDefinition :: Parser OperationDefinition
operationDefinition = OperationSelectionSet <$> selectionSet
                  <|> OperationDefinition   <$> operationType
                                            <*> optional name
                                            <*> opt variableDefinitions
                                            <*> opt directives
                                            <*> selectionSet
                  <?> "operationDefinition error"

operationType :: Parser OperationType
operationType = Query    <$ tok "query"
            <|> Mutation <$ tok "mutation"
            <?> "operationType error"

-- * SelectionSet

selectionSet :: Parser SelectionSet
selectionSet = braces $ manyNE selection

selectionSetOpt :: Parser SelectionSetOpt
selectionSetOpt = braces $ many1 selection

selection :: Parser Selection
selection = SelectionField          <$> field
        <|> SelectionFragmentSpread <$> fragmentSpread
        <|> SelectionInlineFragment <$> inlineFragment
        <?> "selection error!"

-- * Field

field :: Parser Field
field = Field <$> optional alias
              <*> name
              <*> opt arguments
              <*> opt directives
              <*> opt selectionSetOpt

alias :: Parser Alias
alias = name <* tok ":"

-- * Arguments

arguments :: Parser Arguments
arguments = parens $ many1 argument

argument :: Parser Argument
argument = Argument <$> name <* tok ":" <*> value

-- * Fragments

fragmentSpread :: Parser FragmentSpread
fragmentSpread = FragmentSpread <$  tok "..."
                                <*> fragmentName
                                <*> opt directives

inlineFragment :: Parser InlineFragment
inlineFragment = InlineFragment <$  tok "..."
                                <*> optional typeCondition
                                <*> opt directives
                                <*> selectionSet

fragmentDefinition :: Parser FragmentDefinition
fragmentDefinition = FragmentDefinition
                 <$  tok "fragment"
                 <*> name
                 <*> typeCondition
                 <*> opt directives
                 <*> selectionSet

fragmentName :: Parser FragmentName
fragmentName = but (tok "on") *> name

typeCondition :: Parser TypeCondition
typeCondition = tok "on" *> name

-- * Input Values

value :: Parser Value
value = ValueVariable <$> variable
    <|> tok (either ValueFloat ValueInt . floatingOrInteger <$> scientific)
    <|> ValueBoolean  <$> booleanValue
    <|> ValueNull     <$  tok "null"
    <|> ValueString   <$> stringValue
    <|> ValueEnum     <$> enumValue
    <|> ValueList     <$> listValue
    <|> ValueObject   <$> objectValue
    <?> "value error!"
  where
    booleanValue :: Parser Bool
    booleanValue = True  <$ tok "true"
               <|> False <$ tok "false"

    -- TODO: Escape characters. Look at `jsstring_` in aeson package.
    stringValue :: Parser Text
    stringValue = quotes (takeWhile (/= '"'))

    enumValue :: Parser Name
    enumValue = but (tok "true") *> but (tok "false") *> but (tok "null") *> name

    listValue :: Parser [Value]
    listValue = brackets $ many1 value

    objectValue :: Parser [ObjectField]
    objectValue = braces $ many1 objectField

objectField :: Parser ObjectField
objectField = ObjectField <$> name <* tok ":" <*> value

-- * Variables

variableDefinitions :: Parser VariableDefinitions
variableDefinitions = parens $ many1 variableDefinition

variableDefinition :: Parser VariableDefinition
variableDefinition = VariableDefinition <$> variable
                                        <*  tok ":"
                                        <*> type_
                                        <*> optional defaultValue

variable :: Parser Variable
variable = tok "$" *> name

defaultValue :: Parser DefaultValue
defaultValue = tok "=" *> value

-- * Input Types

type_ :: Parser Type
type_ = TypeNamed   <$> name
    <|> TypeList    <$> brackets type_
    <|> TypeNonNull <$> nonNullType
    <?> "type_ error!"

nonNullType :: Parser NonNullType
nonNullType = NonNullTypeNamed <$> name <* tok "!"
          <|> NonNullTypeList  <$> brackets type_  <* tok "!"
          <?> "nonNullType error!"

-- * Directives

directives :: Parser Directives
directives = many1 directive

directive :: Parser Directive
directive = Directive
        <$  tok "@"
        <*> name
        <*> opt arguments

-- * Internal

tok :: Parser a -> Parser a
tok p = p <* whiteSpace

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

quotes :: Parser a -> Parser a
quotes = between "\"" "\""

brackets :: Parser a -> Parser a
brackets = between "[" "]"

between :: Parser Text -> Parser Text -> Parser a -> Parser a
between open close p = tok open *> p <* tok close

opt :: Monoid a => Parser a -> Parser a
opt = option mempty

-- Hack to reverse parser success
but :: Parser a -> Parser ()
but pn = False <$ lookAhead pn <|> pure True >>= \case
  False -> empty
  True  -> pure ()

manyNE :: Alternative f => f a -> f (NonEmpty a)
manyNE p = (:|) <$> p <*> many p

whiteSpace :: Parser ()
whiteSpace = peekChar >>= traverse_ (\c ->
    if isSpace c || c == ','
       then anyChar *> whiteSpace
       else when (c == '#') $ manyTill anyChar endOfLine *> whiteSpace)
