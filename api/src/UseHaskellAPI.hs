{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module UseHaskellAPI where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import           Data.Time.Clock              (UTCTime(..))
import           GHC.Generics
import           Servant

data User = User { name :: String
                 , pass :: String
                 } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data LoginRequest = LoginRequest { user :: String
                                 , request :: String
                                 } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data Token = Token { success :: Bool
                   , ticket :: String
                   , encSessionKey :: String
                   , encEncSessionKey :: String
                   , timeout :: Int
                   , serverHost :: String
                   , serverPort :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data DownloadRequest = DownloadRequest { encUser :: String  -- encrypted with session key
                                       , secretTicket :: String -- encrypted with auth secret key
                                       , encSessionkey :: String  -- encrypted with auth secret key
                                       , encFullPath :: String  -- encrypted with session key
                                       , timeout2 :: Int
                                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data DownloadResponse = DownloadResponse { encStatus :: String  -- encrypted with session key
                                         , encContents :: String  -- encrypted with session key
                                         } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String
deriving instance ToBSON   String
deriving instance FromBSON Bool
deriving instance ToBSON   Bool
deriving instance FromBSON Int
deriving instance ToBSON   Int

type AuthAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token

type DirAPI = "download" :> ReqBody '[JSON] DownloadRequest :> Post '[JSON] DownloadResponse
