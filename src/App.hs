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
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Persist.Postgresql (ConnectionPool, SqlBackend, createPostgresqlPool, entityVal,
                                    runSqlPersistMPool, selectFirst, selectList, (==.))
import Models
import Network.Wai.Handler.Warp as Warp
import Servant
import System.Environment

server :: ConnectionPool -> Server Api
server pool =
  allcompaniesGetH :<|>
  companyGetH :<|>
  allusersGetH :<|>
  userGetH :<|>
  serveDirectoryFileServer "static"
  where
    allcompaniesGetH  = liftIO $ allcompaniesGet
    companyGetH title = liftIO $ companyGet title
    allusersGetH      = liftIO $ allusersGet
    userGetH name     = liftIO $ userGet name

    allcompaniesGet :: IO [Company]
    allcompaniesGet = flip runSqlPersistMPool pool $ do
      companies <- selectList [] []
      return $ map entityVal companies

    companyGet :: Text -> IO (Maybe Company)
    companyGet title = flip runSqlPersistMPool pool $ do
      mcompany <- selectFirst [CompanyTitle ==. title] []
      return $ entityVal <$> mcompany

    allusersGet :: IO [User]
    allusersGet = flip runSqlPersistMPool pool $ do
      users <- selectList [] []
      return $ map entityVal users

    userGet :: Text -> IO (Maybe User)
    userGet name = flip runSqlPersistMPool pool $ do
      mUser <- selectFirst [UserName ==. Just name] []
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
