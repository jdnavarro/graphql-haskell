{-# LANGUAGE OverloadedStrings #-}
-- | This module defines a printer for the @GraphQL@ language.
module Data.GraphQL.Encoder where

import Data.Monoid ((<>))

import Data.Text (Text, cons, intercalate, pack, snoc)

import Data.GraphQL.AST

-- * Document

-- TODO: Use query shorthand
document :: Document -> Text
document (Document defs) = (`snoc` '\n') . mconcat $ definition <$> defs

definition :: Definition -> Text
definition (DefinitionOperation x) = operationDefinition x
definition (DefinitionFragment  x) = fragmentDefinition x

operationDefinition :: OperationDefinition -> Text
operationDefinition (Query    n) = "query "    <> node n
operationDefinition (Mutation n) = "mutation " <> node n

node :: Node -> Text
node (Node name vds ds ss) =
       name
    <> optempty variableDefinitions vds
    <> optempty directives ds
    <> selectionSet ss

variableDefinitions :: [VariableDefinition] -> Text
variableDefinitions = parensCommas variableDefinition

variableDefinition :: VariableDefinition -> Text
variableDefinition (VariableDefinition var ty dv) =
    variable var <> ":" <> type_ ty <> maybe mempty defaultValue dv

defaultValue :: DefaultValue -> Text
defaultValue val = "=" <> value val

variable :: Variable -> Text
variable (Variable name) = "$" <> name

selectionSet :: SelectionSet -> Text
selectionSet = bracesCommas selection

selection :: Selection -> Text
selection (SelectionField          x) = field x
selection (SelectionInlineFragment x) = inlineFragment x
selection (SelectionFragmentSpread x) = fragmentSpread x

field :: Field -> Text
field (Field alias name args ds ss) =
       optempty (`snoc` ':') alias
    <> name
    <> optempty arguments args
    <> optempty directives ds
    <> optempty selectionSet ss

arguments :: [Argument] -> Text
arguments = parensCommas argument

argument :: Argument -> Text
argument (Argument name v) = name <> ":" <> value v

-- * Fragments

fragmentSpread :: FragmentSpread -> Text
fragmentSpread (FragmentSpread name ds) =
    "..." <> name <> optempty directives ds

inlineFragment :: InlineFragment -> Text
inlineFragment (InlineFragment (NamedType tc) ds ss) =
    "... on " <> tc
              <> optempty directives ds
              <> optempty selectionSet ss

fragmentDefinition :: FragmentDefinition -> Text
fragmentDefinition (FragmentDefinition name (NamedType tc) ds ss) =
    "fragment " <> name <> " on " <> tc
                <> optempty directives ds
                <> selectionSet ss

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
stringValue :: Text -> Text
stringValue = quotes

listValue :: ListValue -> Text
listValue (ListValue vs) = bracketsCommas value vs

objectValue :: ObjectValue -> Text
objectValue (ObjectValue ofs) = bracesCommas objectField ofs

objectField :: ObjectField -> Text
objectField (ObjectField name v) = name <> ":" <> value v

-- * Directives

directives :: [Directive] -> Text
directives = spaces directive

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
listType (ListType ty) = brackets (type_ ty)

nonNullType :: NonNullType -> Text
nonNullType (NonNullTypeNamed (NamedType x)) = x <> "!"
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
