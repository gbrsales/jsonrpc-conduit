{- |
Module      :  Data.Conduit.JsonRpc.Internal.Types
Description :  Types used internally to encode requests and responses.
Copyright   :  (c) 2015 Gabriele Sales
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.JsonRpc.Internal.Types
  ( Request(..)
  , Response(..) )
where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson          hiding (Error)
import           Data.Aeson.Types    (emptyArray)
import           Data.Monoid         as M (mempty)
import           Data.Text           (Text)


data Request a = Request { reqMethod :: Text
                         , reqParams :: a
                         , reqId     :: Value }

instance FromJSON (Request Value) where
  parseJSON (Object v) = do
    version <- v .: "jsonrpc"
    guard (version == ("2.0" :: Text))
    Request <$> v .:  "method" <*>
                (v .:? "params") .!= emptyArray <*>
                v .:  "id"

  parseJSON _ = M.mempty

instance ToJSON a => ToJSON (Request a) where
  toJSON (Request m ps id) =
    object [ "jsonrpc" .= ("2.0" :: Text)
           , "method"  .= m
           , "params"  .= toJSON ps
           , "id"      .= id ]


data Response a = Result { result   :: a
                         , resultId :: Value }
                | Error  { errCode  :: Int
                         , errMsg   :: Text
                         , errRefId :: Maybe Value }
  deriving (Show)

instance FromJSON a => FromJSON (Response a) where
  parseJSON (Object v) = do
    version <- v .: "jsonrpc"
    guard (version == ("2.0" :: Text))
    fromResult <|> fromError

    where
      fromResult = Result <$> (v .: "result" >>= parseJSON)
                          <*> v .: "id"

      fromError = do
        err <- v .: "error"
        Error <$> err .: "code"
              <*> err .: "message"
              <*> v   .: "id"

  parseJSON _ = mempty

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
