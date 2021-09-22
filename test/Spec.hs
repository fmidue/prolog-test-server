{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Header
import Data.Aeson (Value(..), object, (.=))

import Server (app)

main :: IO ()
main = hspec spec

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200" $ do
      get "/" `shouldRespondWith` 200

    it "has 'Content-Type: text/html; charset=utf-8'" $ do
      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/html; charset=utf-8"]}
