{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Docs where

import Api
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time
       (UTCTime(..), fromGregorian, secondsToDiffTime, utctDay)
import Database.Persist.Sql (toSqlKey)
import Models
import Servant.API
import Servant.Docs

instance ToCapture (Capture "name" Text) where
  toCapture _ =
    DocCapture "name"
               "Name of the user"

instance ToCapture (Capture "users" Text) where
  toCapture _ =
    DocCapture "users"
               "Get all users"

instance ToCapture (Capture "title" Text) where
  toCapture _ =
    DocCapture "title"
               "Title of the company"

instance ToSample User where
  toSamples _ = singleSample mkUser


instance ToParam (QueryParam "start" UTCTime) where
  toParam _ =
    DocQueryParam "start"
                  ["2017-10-11T21%3A12%3A13.120Z", "2018-10-11T21%3A12%3A13.120Z", "..."]
                  "Start date for the historical data"
                  Normal

instance ToParam (QueryParam "end" UTCTime) where
  toParam _ =
    DocQueryParam "end"
                  ["2017-10-11T21%3A12%3A13.120Z", "2018-10-11T21%3A12%3A13.120Z", "..."]
                  "End date for the historical data"
                  Normal

instance ToSample Historical where
  toSamples _ =
    [ ("When you provide start date and an end date you receive data between that range"
    , mkHistorical)
    , ("When date range is not specified you receive all historical data"
    , mkHistorical)]

instance ToSample Company where
  toSamples _ =
    [ ("When you provide Company name you receive single company data"
    , mkCompany)
    , ("When company name is not specified you receive all company data"
    , mkCompany)]

instance ToSample Story where
  toSamples _ =
    [("Returns all story data"
    , mkStory)]

mkCompany :: Company
mkCompany =
  Company
    "Company title"
    (Just "https://companytitle.com")
    (Just "Some company description")
    Nothing
    "AAPL"
    Nothing
    Nothing
    mkUtctime

mkStory :: Story
mkStory =
  Story
    1
    "Some story title"
    "https://test.com/story"
    (Just "Some story content")
    Nothing
    mkUtctime

mkHistorical :: Historical
mkHistorical =
  Historical
    (toSqlKey 1)
    "AAPL"
    mkUtctime
    100.00
    100.00
    200.00
    50.00
    150.00
    1234000

mkUser :: User
mkUser =
  User
    "test@email.com"
    (Just "Name")
    (Just "Lastname")
    Nothing
    (Just "SRB")
    (Just "Belgrade")
    Nothing
    mkUtctime


mkUtctime :: UTCTime
mkUtctime =
  UTCTime
  {utctDay = fromGregorian 2018 1 1
  , utctDayTime = secondsToDiffTime 2000}

apiDocs :: API
apiDocs = docs api

docsBS :: ByteString
docsBS = encodeUtf8 . pack . markdown $ docsWithIntros [intro] docsApi
  where
    intro =
      DocIntro
        "Investments-info API service"
        [ "Investments-info API v0.0.1"
        , "All endpoints are available for free. Enjoy!"
        ]
