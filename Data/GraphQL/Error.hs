{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Error (
  queryError,
  parseError
  ) where

import Prelude hiding (error)

import Control.Applicative (Alternative, pure)

import qualified Data.Aeson as Aeson
import Data.Text

import Data.GraphQL.AST
import Data.GraphQL.Schema (Schema(..))
import qualified Data.GraphQL.Schema as Schema

import Data.GraphQL.Encoder (document)

import Debug.Trace

{- | queryError is called when the schema returns no data,
     and returns an error message.
-}
queryError ::
  Alternative f =>
  Schema.Schema f -> Schema.Subs -> Document -> f Aeson.Value
queryError schema@(Schema resolvs) subs doc =
  pure $  Aeson.object [("errors", Aeson.String errs )]
  where errs = document $ traceShowId doc


parseError :: (Alternative m, Monad m) => String -> m Aeson.Value
parseError s = pure $ Aeson.object [("errors", Aeson.toJSON [Aeson.object [("message", Aeson.toJSON s)]])]
