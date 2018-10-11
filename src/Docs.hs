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
import Data.Proxy
import Data.Text (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time
       (UTCTime(..), fromGregorian, secondsToDiffTime, utctDay)
import Database.Persist.Sql (toSqlKey)
import Models
import Network.HTTP.Types
import Network.Wai
import Servant.API
import Servant.Docs
import Servant.Server
import Web.FormUrlEncoded (FromForm(..), ToForm(..))

instance ToCapture (Capture "user" Text) where
  toCapture _ =
    DocCapture "user"
               "Name of the user"

instance ToCapture (Capture "users" Text) where
  toCapture _ =
    DocCapture "users"
               "Get all users"

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
