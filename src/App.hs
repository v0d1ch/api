{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import Api
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.ByteString.Char8 (pack)
import Data.Int (Int64)
import Data.Pool (Pool)
import Database.Persist.Postgresql (ConnectionPool, SqlBackend, createPostgresqlPool, entityVal,
                                    runSqlPersistMPool, selectList, selectFirst, toSqlKey, (==.))
import Models
import Network.Wai.Handler.Warp as Warp
import Servant
import System.Environment

server :: ConnectionPool -> Server Api
server pool =
  allusersGetH :<|>
  userGetH :<|>
  serveDirectoryFileServer "static"
  where
    allusersGetH     = liftIO $ allusersGet
    userGetH name    = liftIO $ userGet name

    allusersGet :: IO [User]
    allusersGet = flip runSqlPersistMPool pool $ do
      users <- selectList [] []
      return $ map entityVal users

    userGet :: Int64 -> IO (Maybe User)
    userGet uid = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserId ==. toSqlKey uid] []
      return $ entityVal <$> mUser

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: IO Application
mkApp = do
  db    <- getEnv "iiservant"
  pool  <- runStdoutLoggingT $ createPostgresqlPool (pack db) 10 :: IO (Pool SqlBackend)
  return $ app pool

run :: IO ()
run = Warp.run 3000 =<< mkApp
