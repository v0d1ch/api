{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Data.Text
import Data.Time (UTCTime)
import Models
import Servant.API

type Api =
  -- "http://localhost:3000/historical-range/?start=2017-10-11T21%3A12%3A13.120Z&end=2018-10-11T21%3A12%3A13.120Z
  "historical-range"
     :> QueryParam "start" UTCTime
     :> QueryParam "end" UTCTime
     :> Get '[JSON] [Historical]
  :<|> "historical" :> Get '[JSON] [Historical]
  :<|> "stories"    :> Get '[JSON] [Story]
  :<|> "companies"  :> Get '[JSON] [Company]
  :<|> "company"    :> Capture "title" Text :> Get '[JSON] (Maybe Company)
  :<|> "users"      :> Get '[JSON] [User]
  :<|> "user"       :> Capture "name" Text :> Get '[JSON] (Maybe User)
  :<|> Raw

api :: Proxy Api
api = Proxy
