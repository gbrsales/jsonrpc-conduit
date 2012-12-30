-- |
-- Module      : Network.JsonRpc.Methods
-- Copyright   : (c) 2012 Gabriele Sales <gbrsales@gmail.com>
-- Maintainer  : Gabriele Sales
--
-- JSON-RPC methods and their registry.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.JsonRpc.Methods
  ( MethodError(..)
  , Method(..)
  , method

  , Methods
  , fromList
  , lookup )
where

import           Data.Aeson
import qualified Data.HashMap.Strict as M
import           Data.Text           (Text)
import           Prelude             hiding (lookup)


data MethodError = MethodError !Int !Text
  deriving (Eq, Show)

data Method m where
  Method :: forall i m o. (FromJSON i, ToJSON o)
         => (i -> m (Either MethodError o)) -> Method m

method :: (FromJSON i, ToJSON o)
       => Text
       -> (i -> m (Either MethodError o))
       -> (Text, Method m)
method name f = (name, Method f)


newtype Methods m = Methods (M.HashMap Text (Method m))

fromList :: [(Text, Method m)] -> Methods m
fromList = Methods . M.fromList

lookup :: Methods m -> Text -> Maybe (Method m)
lookup (Methods m) name = M.lookup name m
