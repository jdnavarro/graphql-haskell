{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.GraphQL.Error (
  parseError,
  CollectErrsT,
  addErr,
  addErrMsg,
  runCollectErrs,
  joinErrs,
  errWrap
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text, pack)

import Control.Arrow ((&&&))

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative, pure)
import Data.Foldable (Foldable, concatMap)
import Prelude hiding (concatMap)
#endif

-- | Wraps a parse error into a list of errors.
parseError :: Applicative f => String -> f Aeson.Value
parseError s =
  pure $ Aeson.object [("errors", Aeson.toJSON [makeErrorMsg $ pack s])]

-- | A wrapper for an applicative functor, for passing around error messages.
type CollectErrsT f a = f (a,[Aeson.Value])

-- | Takes a (wrapped) list (foldable functor) of values and errors and
--   joins the values into a list and concatenates the errors.
joinErrs
  :: (Functor m, Functor f, Foldable f)
  => m (f (a,[Aeson.Value])) -> CollectErrsT m (f a)
joinErrs = fmap $ fmap fst &&& concatMap snd

-- | Wraps the given applicative to handle errors
errWrap :: Functor f => f a -> f (a, [Aeson.Value])
errWrap = fmap (flip (,) [])

-- | Adds an error to the list of errors.
addErr :: Functor f => Aeson.Value -> CollectErrsT f a -> CollectErrsT f a
addErr v = (fmap . fmap) (v :)

makeErrorMsg :: Text -> Aeson.Value
makeErrorMsg s = Aeson.object [("message",Aeson.toJSON s)]

-- | Convenience function for just wrapping an error message.
addErrMsg :: Functor f => Text -> CollectErrsT f a -> CollectErrsT f a
addErrMsg = addErr . makeErrorMsg

-- | Runs the given query computation, but collects the errors into an error
--  list, which is then sent back with the data.
runCollectErrs :: Functor f => CollectErrsT f Aeson.Value -> f Aeson.Value
runCollectErrs = fmap finalD
  where finalD (dat,errs) =
          Aeson.object $
          if null errs
            then [("data",dat)]
            else [("data",dat),("errors",Aeson.toJSON $ reverse errs)]
