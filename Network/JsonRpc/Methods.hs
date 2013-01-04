-- |
-- Module      : Network.JsonRpc.Methods
-- Copyright   : (c) 2012 Gabriele Sales <gbrsales@gmail.com>
-- Maintainer  : Gabriele Sales
--
-- JSON-RPC methods and their registry.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.JsonRpc.Methods
  ( Method(..)
  , MethodError(..)

  , NamedMethod
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

newtype NamedMethod m = NamedMethod { unWrap :: (Text, Method m) }

method :: (FromJSON i, ToJSON o)
       => Text
       -> (i -> m (Either MethodError o))
       -> NamedMethod m
method name f = NamedMethod (name, Method f)

newtype Methods m = Methods (M.HashMap Text (Method m))

-- | Builds a collection from a list of 'NamedMethod's.
fromList :: [NamedMethod m] -> Methods m
fromList = Methods . M.fromList . map unWrap

lookup :: Methods m -> Text -> Maybe (Method m)
lookup (Methods m) name = M.lookup name m
