module Data.GraphQL.AST.Transform where

import Control.Applicative (empty)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
-- import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid (Alt(Alt,getAlt))
import Data.Foldable (foldMap)

import Data.Text (Text)

import qualified Data.GraphQL.AST as Full
import qualified Data.GraphQL.AST.Core as Core
import qualified Data.GraphQL.Schema as Schema

type Name = Text

-- | Replaces a fragment name by a list of 'Field'. If the name doesn't match an
--   empty list is returned.
type Fragmenter = Name -> [Core.Field]

document :: Schema.Subs -> Full.Document -> Core.Document
document subs defs = operations subs fr ops
  where
    (fr, ops) = first foldFrags . partitionEithers . NonEmpty.toList $ defrag <$> defs

    foldFrags :: [Fragmenter] -> Fragmenter
    foldFrags fs n = getAlt $ foldMap (Alt . ($ n)) fs

-- * Fragment replacement

-- | Extract Fragments into a single Fragmenter function and a Operation
--   Definition.
defrag :: Full.Definition -> Either Fragmenter Full.OperationDefinition
defrag (Full.DefinitionOperation op) = Right op
defrag (Full.DefinitionFragment fragDef) = Left $ fragmentDefinition fragDef

fragmentDefinition :: Full.FragmentDefinition -> Fragmenter
fragmentDefinition (Full.FragmentDefinition name _tc _dirs sels) name' =
  if name == name' then NonEmpty.toList (selection <$> sels) else empty

selection :: Full.Selection -> Core.Field
selection (Full.SelectionField _fld)       = field _fld
selection (Full.SelectionFragmentSpread _) = error "Nested fragments not supported yet"
selection (Full.SelectionInlineFragment _) =
  error "Inline fragments within fragments not supported yet"

field :: Full.Field -> Core.Field
field (Full.Field a n args _ sels) =
  Core.Field a n (argument <$> args) (selection <$> sels)

argument :: Full.Argument -> Core.Argument
argument (Full.Argument n v) = Core.Argument n (value v)

value :: Full.Value -> Core.Value
value (Full.ValueVariable _) = error "Variables within fragments not supported yet"
value (Full.ValueInt      i) = Core.ValueInt i
value (Full.ValueFloat    f) = Core.ValueFloat f
value (Full.ValueString   x) = Core.ValueString x
value (Full.ValueBoolean  b) = Core.ValueBoolean b
value  Full.ValueNull        = Core.ValueNull
value (Full.ValueEnum     e) = Core.ValueEnum e
value (Full.ValueList     l) = Core.ValueList (value <$> l)
value (Full.ValueObject   o) = Core.ValueObject (objectField <$> o)

objectField :: Full.ObjectField -> Core.ObjectField
objectField (Full.ObjectField n v) = Core.ObjectField n (value v)

-- * Operation

operations
  :: Schema.Subs
  -> Fragmenter
  -> [Full.OperationDefinition]
  -> Core.Document
-- XXX: Replace `fromList` by proper error: at least a Query or Mutation
-- operation must be present
operations subs fr = NonEmpty.fromList . fmap (operation subs fr)

operation
  :: Schema.Subs
  -> Fragmenter
  -> Full.OperationDefinition
  -> Core.Operation
operation _subs _fr _op = undefined
