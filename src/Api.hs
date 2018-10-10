{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Models
import Data.Text
import Servant.API

type Api =
       "stories"   :> Get '[JSON] [Story]
  :<|> "companies" :> Get '[JSON] [Company]
  :<|> "company"   :> Capture "title" Text :> Get '[JSON] (Maybe Company)
  :<|> "users"     :> Get '[JSON] [User]
  :<|> "user"      :> Capture "name" Text :> Get '[JSON] (Maybe User)
  :<|> Raw

api :: Proxy Api
api = Proxy
