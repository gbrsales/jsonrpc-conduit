-- |
-- Module      : Data.Conduit.JsonRpc.Methods
-- Copyright   : (c) 2012-2013 Gabriele Sales <gbrsales@gmail.com>
--
-- JSON-RPC methods.

{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Data.Conduit.JsonRpc.Methods
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


{-|
A wrapper over a monadic function that can either succeed or fail with a
'MethodError'.

Hides the input and output types.
-}
data Method m where
  Method :: forall i m o. (FromJSON i, ToJSON o)
         => (i -> m (Either MethodError o)) -> Method m

-- | Represents an error with an integer code and a textual message.
data MethodError = MethodError !Int !Text
  deriving (Eq, Show)


-- | A 'Method' with a name.
newtype NamedMethod m = NamedMethod { unWrap :: (Text, Method m) }

{-|
Builds a 'NamedMethod' given its name and function.

Useful in conjuction with 'fromList'.
-}
method :: (FromJSON i, ToJSON o)
       => Text
       -> (i -> m (Either MethodError o))
       -> NamedMethod m
method name f = NamedMethod (name, Method f)

-- | Collection of 'NamedMethod's.
newtype Methods m = Methods (M.HashMap Text (Method m))

-- | Builds a collection from a list of 'NamedMethod's.
fromList :: [NamedMethod m] -> Methods m
fromList = Methods . M.fromList . map unWrap

-- | Looks up the method corresponding to the given name.
lookup :: Methods m -> Text -> Maybe (Method m)
lookup (Methods m) name = M.lookup name m
