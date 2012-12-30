-- |
-- Copyright   : (c) 2012 Gabriele Sales <gbrsales@gmail.com>
-- Maintainer  : Gabriele Sales
--
-- An example JSON-RPC server implementing the "echo" and "sum" methods.

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main )
where

import           Data.Aeson
import           Data.ByteString         (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import           Network.JsonRpc.Methods
import           Network.JsonRpc.Server
import           System.IO


main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  sourceHandle stdin $= server $$ sinkHandle stdout

server :: Conduit ByteString IO ByteString
server = serve (fromList [ method "echo" echo
                         , method "sum"  sum' ])

echo :: Value -> IO (Either MethodError Value)
echo = return . Right . id

sum' :: [Int] -> IO (Either MethodError Int)
sum' = return . Right . sum
