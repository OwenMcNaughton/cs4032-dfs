{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}


module RestClient where

import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           GHC.Generics
import           Network.HTTP.Client (defaultManagerSettings, newManager)
import           Servant.API
import           Servant.Client

type Username = Text

data UserSummary = UserSummary
  { summaryUsername :: Username
  , summaryUserid   :: Int
  } deriving (Eq, Show)

instance FromJSON UserSummary where
  parseJSON (Object o) =
    UserSummary <$> o .: "username"
                <*> o .: "userid"

  parseJSON _ = mzero

type Group = Text

data UserDetailed = UserDetailed
  { username :: Username
  , userid   :: Int
  , groups   :: [Group]
  } deriving (Eq, Show, Generic, FromJSON)

data Package = Package { packageName :: Text }
  deriving (Eq, Show, Generic, FromJSON)

type HackageAPI = "users" :> Get '[JSON] [UserSummary]
             :<|> "user" :> Capture "username" Username :> Get '[JSON] UserDetailed
             :<|> "packages" :> Get '[JSON] [Package]

hackageAPI :: Proxy HackageAPI
hackageAPI = Proxy

getUsers :: ClientM  [UserSummary]
getUser :: Username -> ClientM UserDetailed
getPackages :: ClientM  [Package]

getUsers :<|> getUser :<|> getPackages = client hackageAPI
