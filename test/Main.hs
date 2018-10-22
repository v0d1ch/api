{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import App (app)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString.Char8 (pack)
import System.Environment
import Test.Hspec
import Test.Hspec.Wai

main :: IO ()
main = do
  db    <- getEnv "iiservant"
  pool  <- runStdoutLoggingT $ createPostgresqlPool (pack db) 10
  hspec $ spec pool

spec :: ConnectionPool -> Spec
spec pool = do
  with (return (app pool)) $ do
    describe "GET /" $ do
        it "responds with 200" $ do
            get "/" `shouldRespondWith` 200
