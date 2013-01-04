-- |
-- Module      : Network.JsonRpc.Server
-- Copyright   : (c) 2012 Gabriele Sales <gbrsales@gmail.com>
-- Maintainer  : Gabriele Sales
--
-- JSON-RPC 2.0 server.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.JsonRpc.Server
  ( serve )
where

import           Control.Applicative
import           Control.Monad              (guard, (>=>))
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.State
import           Data.Aeson                 hiding (Error)
import           Data.Aeson.Types           (emptyArray, parseMaybe)
import           Data.Attoparsec.ByteString
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as L
import           Data.Conduit
import qualified Data.Conduit.List          as C
import           Data.Monoid
import           Data.Text                  (Text)
import           Network.JsonRpc.Methods    hiding (method)
import           Prelude                    hiding (lookup)


data Request a = Request { method    :: Text
                         , params    :: a
                         , requestId :: Value }

instance FromJSON (Request Value) where
    parseJSON (Object v) = do
      version <- v .: "jsonrpc"
      guard (version == ("2.0" :: Text))
      Request <$> v .:  "method" <*>
                  (v .:? "params") .!= emptyArray <*>
                  v .:  "id"

    parseJSON _ = mempty


data Processed a = Correct !a
                 | InvalidRequest
                 | ParseError


data Response a = Result { result   :: a
                         , resultId :: Value }
                | Error  { code    :: Int
                         , message :: Text
                         , refId   :: Maybe Value }
  deriving (Show)

instance ToJSON (Response Value) where
    toJSON (Result x id) = object [ "jsonrpc" .= ("2.0" :: Text)
                                  , "result"  .= x
                                  , "id"      .= id ]
    toJSON (Error code msg id) =
        let err = object [ "code"    .= code
                         , "message" .= msg ]
        in object [ "jsonrpc" .= ("2.0" :: Text)
                  , "error"   .= err
                  , "id"      .= id ]


serve :: (Applicative m, Monad m) => Methods m -> Conduit ByteString m ByteString
serve methods = parseRequests =$=
                C.concatMapM (\r -> encodeResponse <$> handleRequest methods r)


parseRequests :: (Monad m)
              => Pipe ByteString ByteString (Processed (Request Value)) () m ()
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

    handle (Fail {})     = lift (yield ParseError)
    handle (Partial k)   = put (Just k) >> loop
    handle (Done rest r) = do
      lift (yieldResponse r)
      if B.null rest
         then loop
         else process rest

    yieldResponse r = yield $ case parseMaybe parseJSON r of
                                Nothing -> InvalidRequest
                                Just r' -> Correct r'

handleRequest :: (Applicative m, Monad m)
              => Methods m
              -> Processed (Request Value)
              -> m (Response Value)
handleRequest _       InvalidRequest    = invalidRequest
handleRequest _       ParseError        = parseError
handleRequest methods (Correct request) =
  case lookup methods (method request) of
    Nothing -> methodNotFound (requestId request)
    Just m  -> runMethod m request

runMethod :: (Applicative m, Monad m)
          => Method m
          -> Request Value
          -> m (Response Value)
runMethod (Method f) request = do
  let reqId = requestId request
  case parseMaybe parseJSON (params request) of
    Nothing -> invalidParams reqId
    Just ps -> processResult reqId <$> f ps

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
