{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a printer for the @GraphQL@ language.
module Data.GraphQL.Encoder where

import Data.Foldable (fold)
import Data.Monoid ((<>))
import qualified Data.List.NonEmpty as NonEmpty (toList)

import Data.Text (Text, cons, intercalate, pack, snoc)

import Data.GraphQL.AST

-- * Document

document :: Document -> Text
document defs = (`snoc` '\n') . mconcat . NonEmpty.toList $ definition <$> defs

definition :: Definition -> Text
definition (DefinitionOperation x) = operationDefinition x
definition (DefinitionFragment  x) = fragmentDefinition x

operationDefinition :: OperationDefinition -> Text
operationDefinition (OperationSelectionSet sels) = selectionSet sels
operationDefinition (OperationDefinition Query    name vars dirs sels) =
  "query " <> node name vars dirs sels
operationDefinition (OperationDefinition Mutation name vars dirs sels) =
  "mutation " <> node name vars dirs sels

node :: Name -> VariableDefinitions -> Directives -> SelectionSet -> Text
node name vars dirs sels =
       name
    <> optempty variableDefinitions vars
    <> optempty directives dirs
    <> selectionSet sels

variableDefinitions :: [VariableDefinition] -> Text
variableDefinitions = parensCommas variableDefinition

variableDefinition :: VariableDefinition -> Text
variableDefinition (VariableDefinition var ty dv) =
    variable var <> ":" <> type_ ty <> maybe mempty defaultValue dv

defaultValue :: DefaultValue -> Text
defaultValue val = "=" <> value val

variable :: Variable -> Text
variable var = "$" <> var

selectionSet :: SelectionSet -> Text
selectionSet = bracesCommas selection . NonEmpty.toList

selectionSetOpt :: SelectionSetOpt -> Text
selectionSetOpt = bracesCommas selection

selection :: Selection -> Text
selection (SelectionField          x) = field x
selection (SelectionInlineFragment x) = inlineFragment x
selection (SelectionFragmentSpread x) = fragmentSpread x

field :: Field -> Text
field (Field alias name args dirs selso) =
       optempty (`snoc` ':') (fold alias)
    <> name
    <> optempty arguments args
    <> optempty directives dirs
    <> optempty selectionSetOpt selso

arguments :: [Argument] -> Text
arguments = parensCommas argument

argument :: Argument -> Text
argument (Argument name v) = name <> ":" <> value v

-- * Fragments

fragmentSpread :: FragmentSpread -> Text
fragmentSpread (FragmentSpread name ds) =
    "..." <> name <> optempty directives ds

inlineFragment :: InlineFragment -> Text
inlineFragment (InlineFragment tc dirs sels) =
    "... on " <> fold tc
              <> directives dirs
              <> selectionSet sels

fragmentDefinition :: FragmentDefinition -> Text
fragmentDefinition (FragmentDefinition name tc dirs sels) =
    "fragment " <> name <> " on " <> tc
                <> optempty directives dirs
                <> selectionSet sels

-- * Values

value :: Value -> Text
value (ValueVariable x) = variable x
-- TODO: This will be replaced with `decimal` Builder
value (ValueInt      x) = pack $ show x
-- TODO: This will be replaced with `decimal` Builder
value (ValueFloat    x) = pack $ show x
value (ValueBoolean  x) = booleanValue x
value ValueNull         = mempty
value (ValueString   x) = stringValue x
value (ValueEnum     x) = x
value (ValueList     x) = listValue x
value (ValueObject   x) = objectValue x

booleanValue :: Bool -> Text
booleanValue True  = "true"
booleanValue False = "false"

-- TODO: Escape characters
stringValue :: Text -> Text
stringValue = quotes

listValue :: ListValue -> Text
listValue = bracketsCommas value

objectValue :: ObjectValue -> Text
objectValue = bracesCommas objectField

objectField :: ObjectField -> Text
objectField (ObjectField name v) = name <> ":" <> value v

-- * Directives

directives :: [Directive] -> Text
directives = spaces directive

directive :: Directive -> Text
directive (Directive name args) = "@" <> name <> optempty arguments args

-- * Type Reference

type_ :: Type -> Text
type_ (TypeNamed   x) = x
type_ (TypeList    x) = listType x
type_ (TypeNonNull x) = nonNullType x

listType :: Type -> Text
listType x = brackets (type_ x)

nonNullType :: NonNullType -> Text
nonNullType (NonNullTypeNamed x) = x <> "!"
nonNullType (NonNullTypeList  x) = listType x <> "!"

-- * Internal

spaced :: Text -> Text
spaced = cons '\SP'

between :: Char -> Char -> Text -> Text
between open close = cons open . (`snoc` close)

parens :: Text -> Text
parens = between '(' ')'

brackets :: Text -> Text
brackets = between '[' ']'

braces :: Text -> Text
braces = between '{' '}'

quotes :: Text -> Text
quotes = between '"' '"'

spaces :: (a -> Text) -> [a] -> Text
spaces f = intercalate "\SP" . fmap f

parensCommas :: (a -> Text) -> [a] -> Text
parensCommas f = parens . intercalate "," . fmap f

bracketsCommas :: (a -> Text) -> [a] -> Text
bracketsCommas f = brackets . intercalate "," . fmap f

bracesCommas :: (a -> Text) -> [a] -> Text
bracesCommas f = braces . intercalate "," . fmap f

optempty :: (Eq a, Monoid a, Monoid b) => (a -> b) -> a -> b
optempty f xs = if xs == mempty then mempty else f xs
