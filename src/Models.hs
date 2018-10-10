{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models where

import Data.Aeson
import Data.Text
import Database.Persist.TH
import Data.Time (UTCTime)
import Data.Typeable

share [mkPersist sqlSettings] [persistLowerCase|
User json sql=users
    email Text
    name Text Maybe
    lastname Text Maybe
    image Text Maybe
    country Text Maybe
    town Text Maybe
    newsletter Int Maybe
    created_at UTCTime default=current_timestamp
    UniqueUserEmail email
    deriving Eq Show Typeable

Story
    hashId Int
    title Text
    link Text
    content Text Maybe
    image Text Maybe
    created UTCTime default=current_timestamp
    deriving Eq
    deriving Show

Company json
    title Text
    website Text Maybe
    description Text Maybe
    image Text Maybe
    ticker Text
    gicssector Text Maybe
    gicssubindustry Text Maybe
    created UTCTime default=current_timestamp
    deriving Eq
    deriving Show

Historical json
    companyId CompanyId
    ticker Text
    recordDate UTCTime
    recordOpen Double
    recordHigh Double
    recordLow Double
    recordClose Double
    recordAdjClose Double
    recordVolume Int
    deriving Eq
    deriving Show
|]

data Welcome = Welcome Text

instance FromJSON Welcome where
  parseJSON = withObject "Welcome" $ \ v ->
    Welcome <$> v .: "welcome"

instance ToJSON Welcome where
  toJSON (Welcome welcome) =
    object [ "welcome" .= welcome
           ]
