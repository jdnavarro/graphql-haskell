{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
module Data.GraphQL.Execute where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative)
#endif

import qualified Data.Aeson as Aeson

import Data.GraphQL.AST
import Data.GraphQL.Schema

type Response = Aeson.Value

execute :: Schema -> Document -> Maybe Response
execute (Schema resolv0) doc = go resolv0 =<< root doc
  where

    root :: Document -> Maybe Selection
    root (Document [DefinitionOperation (Query (Node _ _ _ [sel]))]) = Just sel
    root _ = error "root: Not implemented yet"

    go :: Resolver -> Selection -> Maybe Response
    go resolv (SelectionField (Field _ n _ _ sfs)) =
        case resolv (InputField n) of
            (OutputScalar s) -> if null sfs
                                   then Just $ Aeson.Object [(n, Aeson.toJSON s)]
                                   else Nothing
            (OutputResolver resolv') -> (\r-> Aeson.Object [(n, r)]) <$> go resolv' (head sfs)
            _ -> error "go case resolv: Not implemented yet"
    go _ _ = error "go: Not implemented yet"
