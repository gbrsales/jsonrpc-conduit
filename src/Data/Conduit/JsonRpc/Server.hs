-- |
-- Module      : Data.Conduit.JsonRpc.Server
-- Copyright   : (c) 2012-2023 Gabriele Sales <gbrsales@gmail.com>
--
-- JSON-RPC 2.0 server 'Conduit'.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.JsonRpc.Server
  ( serve )
where

import           Control.Applicative
import           Control.Monad                       ((>=>))
import           Control.Monad.Trans                 (lift)
import           Control.Monad.Trans.State
import           Data.Aeson                          hiding (Error)
import           Data.Aeson.Parser                   (json')
import           Data.Aeson.Types                    (parseMaybe)
import           Data.Attoparsec.ByteString
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B
import qualified Data.ByteString.Lazy                as L
import           Data.Conduit
import           Data.Conduit.JsonRpc.Internal.Types
import           Data.Conduit.JsonRpc.Methods        hiding (method)
import qualified Data.Conduit.List                   as C
import           Data.Text                           (Text)
import           Prelude                             hiding (lookup)


data Processed a = Correct !a
                 | InvalidRequest
                 | ParseError

{- |
A 'Conduit' that consumes a stream of JSON-RPC requests, tries to process them
with the provided 'Methods' and writes back the results.

Current limitations:

  * does not support batch requests

  * it is not possible to set the @data@ attribute of error objects
-}
serve :: (Applicative m, Monad m)
      => Methods m -> ConduitT ByteString ByteString m ()
serve methods = parseRequests
             .| C.concatMapM (fmap encodeResponse . handleRequest methods)


parseRequests :: (Monad m)
              => ConduitM ByteString (Processed (Request Value)) m ()
parseRequests = evalStateT loop Nothing
  where
    loop = lift await >>= maybe flush process

    process = runParser >=> handle

    flush = do
      p <- get
      case p of
        Nothing -> return ()
        Just k  -> handle (k B.empty)

    runParser chunk = do
      p <- getPartialParser
      return $ case p of
                 Nothing -> parse json' chunk
                 Just k  -> k chunk

    getPartialParser = get <* put Nothing

    handle Fail{}        = lift (yield ParseError)
    handle (Partial k)   = put (Just k) >> loop
    handle (Done rest r) = do
      lift (yieldResponse r)
      if B.null rest
         then loop
         else process rest

    yieldResponse r = yield $ case parseMaybe parseJSON r of
                                Nothing -> InvalidRequest
                                Just r' -> Correct r'

handleRequest :: Monad m
              => Methods m
              -> Processed (Request Value)
              -> m (Response Value)
handleRequest _       InvalidRequest    = invalidRequest
handleRequest _       ParseError        = parseError
handleRequest methods (Correct request) =
  case lookup methods (reqMethod request) of
    Nothing -> methodNotFound (reqId request)
    Just m  -> runMethod m request

runMethod :: (Applicative m, Monad m)
          => Method m
          -> Request Value
          -> m (Response Value)
runMethod (Method f) request = do
  let ri = reqId request
  case parseMaybe parseJSON (reqParams request) of
    Nothing -> invalidParams ri
    Just ps -> processResult ri <$> f ps

processResult :: (ToJSON a) => Value -> Either MethodError a -> Response Value
processResult reqId (Left (MethodError code msg)) = Error code msg (Just reqId)
processResult reqId (Right res)                   = Result (toJSON res) reqId


invalidRequest :: (Monad m) => m (Response Value)
invalidRequest = mkError (-32600) "Invalid request" Nothing

methodNotFound :: (Monad m) => Value -> m (Response Value)
methodNotFound = mkError (-32601) "Method not found" . Just

invalidParams :: (Monad m) => Value -> m (Response Value)
invalidParams = mkError (-32602) "Invalid params" . Just

parseError :: (Monad m) => m (Response Value)
parseError = mkError (-32700) "Parse error" Nothing

mkError :: (Monad m) => Int -> Text -> Maybe Value -> m (Response Value)
mkError code msg id = return (Error code msg id)


encodeResponse :: Response Value -> [ByteString]
encodeResponse = L.toChunks . encode
