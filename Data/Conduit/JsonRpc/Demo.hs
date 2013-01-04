-- |
-- Copyright   : (c) 2012-2013 Gabriele Sales <gbrsales@gmail.com>
--
-- An example JSON-RPC server implementing the "echo" and "sum" methods.

{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main )
where

import           Data.Aeson
import           Data.ByteString              (ByteString)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.JsonRpc.Methods
import           Data.Conduit.JsonRpc.Server
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
