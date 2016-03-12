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


-- | Wraps a parse error into a list of errors.
parseError :: Applicative f => String -> f Aeson.Value
parseError s =
  pure $ Aeson.object [("errors", Aeson.toJSON [makeErrorMsg $ pack s])]

type CollectErrsT f a = f (a,[Aeson.Value])


joinErrs
  :: (Applicative m, Functor f, Foldable f)
  => m (f (a,[Aeson.Value])) -> CollectErrsT m (f a)
joinErrs = fmap $ fmap fst &&& concatMap snd

-- | Wraps the given applicative to handle errors
errWrap :: Applicative f => f a -> f (a, [Aeson.Value])
errWrap = fmap (flip (,) [])

-- | Adds an error to the list of errors.
addErr :: Applicative f => Aeson.Value -> CollectErrsT f a -> CollectErrsT f a
addErr v = (fmap . fmap) (v :)

makeErrorMsg :: Text -> Aeson.Value
makeErrorMsg s = Aeson.object [("message",Aeson.toJSON s)]

-- | Convenience function for just wrapping an error message.
addErrMsg :: Applicative f => Text -> CollectErrsT f a -> CollectErrsT f a
addErrMsg = addErr . makeErrorMsg

-- | Runs the given query computation, but collects the errors into an error
--  list, which is then sent back with the data.
runCollectErrs :: Applicative f => CollectErrsT f Aeson.Value -> f Aeson.Value
runCollectErrs = fmap finalD
  where finalD (dat,errs) =
          Aeson.object $
          if null errs
            then [("data",dat)]
            else [("data",dat),("errors",Aeson.toJSON $ reverse errs)]
