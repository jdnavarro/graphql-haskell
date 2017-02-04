module Data.GraphQL.AST.Transform where

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
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

-- TODO: Replace Maybe by Either CustomError
document :: Schema.Subs -> Full.Document -> Maybe Core.Document
document subs defs = operations subs fr ops
  where
    (fr, ops) = first foldFrags . partitionEithers . NonEmpty.toList $ defrag <$> defs

    foldFrags :: [Fragmenter] -> Fragmenter
    foldFrags fs n = getAlt $ foldMap (Alt . ($ n)) fs

-- * Operation

operations
  :: Schema.Subs
  -> Fragmenter
  -> [Full.OperationDefinition]
  -> Maybe Core.Document
operations subs fr = NonEmpty.nonEmpty <=< traverse (operation subs fr)

operation
  :: Schema.Subs
  -> Fragmenter
  -> Full.OperationDefinition
  -> Maybe Core.Operation
operation subs fr (Full.OperationSelectionSet sels) =
  operation subs fr $ Full.OperationDefinition Full.Query empty empty empty sels
operation _subs fr (Full.OperationDefinition ot _n _vars _dirs sels) =
  case ot of
    Full.Query    -> Core.Query    <$> node
    Full.Mutation -> Core.Mutation <$> node
  where
    node = traverse (hush <=< selection fr) sels

selection :: Fragmenter -> Full.Selection -> Maybe (Either [Core.Field] Core.Field)
selection fr (Full.SelectionField _fld) = Right <$> field fr _fld
selection fr (Full.SelectionFragmentSpread (Full.FragmentSpread n _dirs)) = Just . Left $ fr n
selection _  (Full.SelectionInlineFragment _)  = error "Inline fragments not supported yet"

-- * Fragment replacement

-- | Extract Fragments into a single Fragmenter function and a Operation
--   Definition.
defrag :: Full.Definition -> Either Fragmenter Full.OperationDefinition
defrag (Full.DefinitionOperation op) = Right op
defrag (Full.DefinitionFragment fragDef) = Left $ fragmentDefinition fragDef

fragmentDefinition :: Full.FragmentDefinition -> Fragmenter
fragmentDefinition (Full.FragmentDefinition name _tc _dirs sels) name' =
  -- TODO: Support fragments within fragments. Fold instead of map.
  if name == name'
  then either id pure =<< maybeToList =<< NonEmpty.toList (selection mempty <$> sels)
  else empty

field :: Fragmenter -> Full.Field -> Maybe Core.Field
field fr (Full.Field a n args _ sels) =
    Core.Field a n (argument <$> args) <$> traverse (hush <=< selection fr) sels

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

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
