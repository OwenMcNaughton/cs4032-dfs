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
                   , encTokenSessionKey :: String
                   , encEncSessionKey :: String
                   , tokenTimeout :: Int
                   , tokenServerHost :: String
                   , tokenServerPort :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data DownloadRequest = DownloadRequest { encDlqID :: String  -- encrypted with auth secret key
                                       , encDlqSessionkey :: String  -- encrypted with auth secret key
                                       , dlqTimeout :: Int
                                       } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data DownloadResponse = DownloadResponse { encDlrStatus :: String  -- encrypted with session key
                                         , encContents :: String  -- encrypted with session key
                                         } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data UploadRequest = UploadRequest { encUlqID :: String  -- encrypted with auth secret key
                                   , encUlqSessionkey :: String  -- encrypted with auth secret key
                                   , ulqTimeout :: Int
                                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data UploadResponse = UploadResponse { encUlrStatus :: String  -- encrypted with session key
                                     } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data StatRequest = StatRequest { encStqUser :: String  -- encrypted with session key
                               , encStqTicket :: String  -- encrypted with auth secret key
                               , encStqSessionKey :: String  -- encrypted with auth secret key
                               , encStqFullPath :: String  -- encrypted with session key
                               , stqTimeout :: Int
                               } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data StatResponse = StatResponse { strStatus :: String
                                 , encFullPath :: String  --  encrypted with session key
                                 , encStrServerHost :: String  -- encrypted with session key
                                 , encStrServerPort :: String  -- encrypted with sessionk key
                                 , encStrID :: String  -- encrypted with auth secret key
                                 } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data FileStat = FileStat { fsFullPath :: String
                         , fileID :: String
                         , fileServerNo :: Int
                         , fileServerHost :: String
                         , fileServerPort :: String
                         , owner :: String
                         , private:: Bool
                         } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data ClientFileStat = ClientFileStat { cfsFullPath :: String
                                     , encFileID :: String
                                     , host :: String
                                     , port :: String
                                     } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String
deriving instance ToBSON   String
deriving instance FromBSON Bool
deriving instance ToBSON   Bool
deriving instance FromBSON Int
deriving instance ToBSON   Int

type AuthAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token

type DirAPI = "stat" :> ReqBody '[JSON] StatRequest :> Post '[JSON] StatResponse
  :<|> "fsRegister" :> Get '[JSON] Int
  -- :<|> "lock" :> ReqBody '[JSON] LockRequest :> Post '[JSON] LockResponse

type FsAPI = "download" :> ReqBody '[JSON] DownloadRequest :> Post '[JSON] DownloadResponse
  :<|> "upload" :> ReqBody '[JSON] UploadRequest :> Post '[JSON] UploadResponse
