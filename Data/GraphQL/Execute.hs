{-# LANGUAGE CPP #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif

import qualified Data.Aeson as Aeson (Value)

import Data.GraphQL.AST
import Data.GraphQL.Schema

execute :: Applicative f => Schema -> Document -> f Aeson.Value
execute = undefined
