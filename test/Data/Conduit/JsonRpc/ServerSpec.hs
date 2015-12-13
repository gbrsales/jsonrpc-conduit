{- |
Module      :  Data.Conduit.JsonRpc.ServerSpec
Description :  Tests for the server.
Copyright   :  (c) 2015 Gabriele Sales
-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Conduit.JsonRpc.ServerSpec
  ( spec )
where

import           Data.Aeson                          hiding (Error)
import           Data.ByteString                     (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Binary                 as B
import           Data.Conduit.JsonRpc.Internal.Types
import           Data.Conduit.JsonRpc.Methods
import           Data.Conduit.JsonRpc.Server
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


oneshot :: (ToJSON a, FromJSON b) => Text -> a -> IO (Either Text b)
oneshot method params = do
  let reqId = toJSON (1::Int)
      req = encode (Request method params reqId)
  bs <- B.sourceLbs req =$= server $$ B.sinkLbs
  return $ case decode bs of
             Nothing             -> Left "invalid response"
             Just Error{errMsg}  -> Left errMsg
             Just Result{result} -> Right result

server :: Conduit ByteString IO ByteString
server = serve (fromList [ method "sum" sum'
                         , method "cat" cat ])

sum' :: [Int] -> IO (Either MethodError Int)
sum' = return . Right . sum

cat :: [String] -> IO (Either MethodError String)
cat = return . Right . concat
