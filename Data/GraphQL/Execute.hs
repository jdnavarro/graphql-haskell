{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, (<$>), pure)
#endif
import Control.Applicative (Alternative, empty)

import qualified Data.Aeson as Aeson

import Data.GraphQL.AST
import Data.GraphQL.Schema

type Response = Aeson.Value

execute :: (Alternative f, Monad f) => Schema f -> Document -> f Response
execute (Schema resolv0) doc = go resolv0 =<< root doc
  where
    root :: Applicative f => Document -> f Selection
    root (Document [DefinitionOperation (Query (Node _ _ _ [sel]))]) = pure sel
    root _ = error "root: Not implemented yet"

    go :: (Alternative f, Monad f) => Resolver f -> Selection -> f Response
    go resolv (SelectionField (Field _ n _ _ sfs)) =
        resolv (InputField n) >>= \case
            (OutputScalar s) ->
               if null sfs
                 then (\s' -> Aeson.Object [(n, Aeson.toJSON s')]) <$> s
                 else empty
            (OutputResolver resolv') -> (\r-> Aeson.Object [(n, r)]) <$> go resolv' (head sfs)
            _ -> error "go case resolv: Not implemented yet"
    go _ _ = error "go: Not implemented yet"
