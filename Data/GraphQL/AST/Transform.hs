{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.AST.Transform where

import Control.Applicative (empty)
import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (fold, foldMap)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (maybeToList)
import Data.Monoid (Alt(Alt,getAlt))

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
document subs doc = operations subs fr ops
  where
    (fr, ops) = first foldFrags
              . partitionEithers
              . NonEmpty.toList
              $ defrag subs
            <$> doc

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
-- TODO: Validate Variable definitions with substituter
operation subs fr (Full.OperationDefinition ot _n _vars _dirs sels) =
  case ot of
    Full.Query    -> Core.Query    <$> node
    Full.Mutation -> Core.Mutation <$> node
  where
    node = traverse (hush <=< selection subs fr) sels

selection
  :: Schema.Subs
  -> Fragmenter
  -> Full.Selection
  -> Maybe (Either [Core.Field] Core.Field)
selection subs fr (Full.SelectionField fld) =
  Right <$> field subs fr fld
selection _    fr (Full.SelectionFragmentSpread (Full.FragmentSpread n _dirs)) =
  Just . Left $ fr n
selection _ _  (Full.SelectionInlineFragment _)  =
  error "Inline fragments not supported yet"

-- * Fragment replacement

-- | Extract Fragments into a single Fragmenter function and a Operation
--   Definition.
defrag
  :: Schema.Subs
  -> Full.Definition
  -> Either Fragmenter Full.OperationDefinition
defrag _    (Full.DefinitionOperation op) =
  Right op
defrag subs (Full.DefinitionFragment fragDef) =
  Left $ fragmentDefinition subs fragDef

fragmentDefinition :: Schema.Subs -> Full.FragmentDefinition -> Fragmenter
fragmentDefinition subs (Full.FragmentDefinition name _tc _dirs sels) name' =
  -- TODO: Support fragments within fragments. Fold instead of map.
  if name == name'
  then either id pure =<< maybeToList
                      =<< NonEmpty.toList (selection subs mempty <$> sels)
  else empty

field :: Schema.Subs -> Fragmenter -> Full.Field -> Maybe Core.Field
field subs fr (Full.Field a n args _dirs sels) =
        Core.Field a n (fold $ argument subs `traverse` args)
        -- TODO: hush should error when fragments are not defined in a field
    <$> traverse (hush <=< selection subs fr) (foldr go empty sels)
  where
    go :: Full.Selection -> Full.SelectionSetOpt -> Full.SelectionSetOpt
    go (Full.SelectionFragmentSpread (Full.FragmentSpread name _dirs)) acc =
      (Full.SelectionField . full <$> fr name) ++ acc
    go x acc = x : acc

    full :: Core.Field -> Full.Field
    full (Core.Field a' n' _args' _sels') = Full.Field a' n' empty empty []

argument :: Schema.Subs -> Full.Argument -> Maybe Core.Argument
argument subs (Full.Argument n v) = Core.Argument n <$> value subs v

value :: Schema.Subs -> Full.Value -> Maybe Core.Value
value subs (Full.ValueVariable n) = subs n
value _    (Full.ValueInt      i) = pure $ Core.ValueInt i
value _    (Full.ValueFloat    f) = pure $ Core.ValueFloat f
value _    (Full.ValueString   x) = pure $ Core.ValueString x
value _    (Full.ValueBoolean  b) = pure $ Core.ValueBoolean b
value _     Full.ValueNull        = pure   Core.ValueNull
value _    (Full.ValueEnum     e) = pure $ Core.ValueEnum e
value subs (Full.ValueList     l) =
  Core.ValueList   <$> traverse (value subs) l
value subs (Full.ValueObject   o) =
  Core.ValueObject <$> traverse (objectField subs) o

objectField :: Schema.Subs -> Full.ObjectField -> Maybe Core.ObjectField
objectField subs (Full.ObjectField n v) = Core.ObjectField n <$> value subs v

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just
