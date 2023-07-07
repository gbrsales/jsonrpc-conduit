{- |
Module      :  Data.Conduit.JsonRpc.ServerSpec
Description :  Tests for the server.
Copyright   :  (c) 2015-2023 Gabriele Sales
-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp  #-}

module Data.Conduit.JsonRpc.ServerSpec
  ( spec )
where

import           Control.Monad
import           Data.Aeson                          hiding (Error)
import           Data.Aeson.Parser                   (json')
import           Data.Aeson.Types                    (parseEither)
import           Data.Attoparsec.Combinator          (many')
import           Data.Bifunctor
import qualified Data.ByteString                     as S
import qualified Data.ByteString.Lazy                as L
import           Data.Conduit
import qualified Data.Conduit.Attoparsec             as A
import qualified Data.Conduit.Binary                 as B
import           Data.Conduit.JsonRpc.Internal.Types
import           Data.Conduit.JsonRpc.Methods
import           Data.Conduit.JsonRpc.Server
import           Data.Int
import           Data.Text                           (Text)
import           Test.Hspec


spec :: Spec
spec = do
  describe "method sum" $ do
    it "computes the sum" $ do
      let nums = [1,2,3] :: [Int]
      oneshot "sum" nums `shouldReturn` Right (6::Int)
    it "rejects strings" $ do
      let strings = ["1","2","3"] :: [Text]
      oneshot "sum" strings `shouldReturn`
        (Left "Invalid params" :: Either Text Int)

  describe "method cat" $
    it "concatenates strings" $ do
      let strings = ["a","b","c"] :: [Text]
      oneshot "cat" strings `shouldReturn` Right ("abc"::Text)

  describe "unknown method" $
    it "is rejected" $
      oneshot "unknown" () `shouldReturn`
        (Left "Method not found" :: Either Text ())

  describe "truncated request" $
    it "is rejected" $
      let req = truncatedRequest "sum" ([]::[Text])
      in oneshot' req `shouldReturn`
        (Left "Parse error" :: Either Text ())

  describe "arbitrary JSON value" $
    it "is rejected" $
      let req = encode (toJSON ["a"::Text])
      in oneshot' req `shouldReturn`
        (Left "Invalid request" :: Either Text ())

  describe "multiple requests" $
    it "are processed in order" $
      let reqs = [ ("sum", toJSON [1::Int, 2])
                 , ("cat", toJSON ["a"::Text, "c"])
                 ]
      in multishot reqs `shouldReturn`
           (Right [toJSON (3::Int), toJSON ("ac"::Text)])


oneshot :: (ToJSON a, FromJSON b) => Text -> a -> IO (Either Text b)
oneshot method params =
  let reqId = toJSON (1::Int)
  in  oneshot' (encode $ Request method params reqId)

oneshot' :: FromJSON a => L.ByteString -> IO (Either Text a)
oneshot' req = do
  bs <- runConduit (B.sourceLbs req .| server .| B.sinkLbs)
  return $ case decode bs of
             Nothing             -> Left "invalid response"
             Just Error{errMsg}  -> Left errMsg
             Just Result{result} -> Right result

truncatedRequest :: ToJSON a => Text -> a -> L.ByteString
truncatedRequest method params =
  let reqId = toJSON (2::Int)
      req   = encode (Request method params reqId)
      len   = L.length req
  in L.take (len - 2) req

multishot :: [(Text, Value)] -> IO (Either String [Value])
multishot reqs = do
  let reqs' = [ encode (Request method params (toJSON i))
              | i <- [1::Int ..]
              | (method, params) <- reqs
              ]
      merged = rechunk 10 (L.concat reqs')
  vs <- runConduit
          $ B.sourceLbs merged
         .| server
         .| A.sinkParserEither (many' json')
  return . join . fmap sequence . bimap A.errorMessage (map resultOf) $ vs

rechunk :: Int64 -> L.ByteString -> L.ByteString
rechunk n = L.concat . split
  where
    split bs =
      let (p, s) = L.splitAt n bs
      in if L.null s then [p] else p : split s

resultOf :: Value -> Either String Value
resultOf = parseEither (withObject "message" (.: "result"))


server :: ConduitT S.ByteString S.ByteString IO ()
server = serve (fromList [ method "sum" sum'
                         , method "cat" cat ])

sum' :: [Int] -> IO (Either MethodError Int)
sum' = return . Right . sum

cat :: [String] -> IO (Either MethodError String)
cat = return . Right . concat
