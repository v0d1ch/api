{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Api where

import Data.Proxy
import Models
import Servant.API
import Data.Int (Int64)

type Api =
       "users":> Get '[JSON] [User]
  :<|> "user" :> Capture "id" Int64 :> Get '[JSON] (Maybe User)
  :<|> Raw

api :: Proxy Api
api = Proxy
