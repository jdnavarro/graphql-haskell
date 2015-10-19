{-# LANGUAGE CPP #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif

import qualified Data.Aeson as Aeson (Value)

import Data.GraphQL.AST
import Data.GraphQL.Schema

type Response = Aeson.Value

execute :: Applicative f => Schema f -> Document -> f Response
execute _schema _doc = undefined
