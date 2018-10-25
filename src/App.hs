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
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Postgresql (ConnectionPool, createPostgresqlPool, entityVal,
                                    runSqlPersistMPool, selectFirst, selectList, (<=.), (==.),
                                    (>=.))
import Docs
import Models
import Network.HTTP.Types (ok200)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp as Warp
import Servant
import System.Environment

server :: ConnectionPool -> Server Api
server pool =
  historicalRangeGetH :<|>
  allhistoricalGetH   :<|>
  allstoriesGetH      :<|>
  allcompaniesGetH    :<|>
  companyGetH         :<|>
  allusersGetH        :<|>
  userGetH            :<|>
  Tagged serveDocs
  where
    historicalRangeGetH s e = liftIO $ historicalRangeGet s e
    allhistoricalGetH    = liftIO $ allhistoricalGet
    allstoriesGetH       = liftIO $ allstoriesGet
    allcompaniesGetH     = liftIO $ allcompaniesGet
    companyGetH title    = liftIO $ companyGet title
    allusersGetH         = liftIO $ allusersGet
    userGetH name        = liftIO $ userGet name

    historicalRangeGet :: Maybe UTCTime -> Maybe UTCTime -> IO [Historical]
    historicalRangeGet Nothing _ = return []
    historicalRangeGet _ Nothing = return []
    historicalRangeGet (Just start) (Just end) = flip runSqlPersistMPool pool $ do
      historic <-
        selectList
          [HistoricalRecordDate >=. start
          , HistoricalRecordDate <=. end
          ] []
      return $ map entityVal historic

    allhistoricalGet :: IO [Historical]
    allhistoricalGet = flip runSqlPersistMPool pool $ do
      historic <- selectList [] []
      return $ map entityVal historic

    allstoriesGet :: IO [Story]
    allstoriesGet = flip runSqlPersistMPool pool $ do
      stories <- selectList [] []
      return $ map entityVal stories

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

    serveDocs _ respond =
        respond $ responseLBS ok200 [plain] docsBS
    plain = ("Content-Type", "text/plain")

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: IO Application
mkApp = do
  db    <- getEnv "iiservant"
  pool  <- runStdoutLoggingT $ createPostgresqlPool (pack db) 10
  return $ app pool

run :: IO ()
run = Warp.run 4000 =<< mkApp
