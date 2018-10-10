{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import Api
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, entityVal, insert, runMigration,
                                runSqlPersistMPool, runSqlPool, selectFirst, (==.))
import Models
import Network.Wai.Handler.Warp as Warp
import Servant

server :: ConnectionPool -> Server Api
server pool =
  userAddH :<|>
  userGetH :<|> serveDirectoryFileServer "static"
  where
    userAddH newUser = liftIO $ userAdd newUser
    userGetH name    = liftIO $ userGet name

    userAdd :: User -> IO (Maybe (Key User))
    userAdd newUser = flip runSqlPersistMPool pool $ do
      exists <- selectFirst [UserName ==. (userName newUser)] []
      case exists of
        Nothing -> Just <$> insert newUser
        Just _ -> return Nothing

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. name] []
      return $ entityVal <$> mUser

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ do
    createSqlitePool (cs sqliteFile) 5

  runSqlPool (runMigration migrateAll) pool
  return $ app pool

run :: FilePath -> IO ()
run sqliteFile =
  Warp.run 3000 =<< mkApp sqliteFile
