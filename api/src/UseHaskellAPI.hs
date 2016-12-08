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

data LoginRequest = LoginRequest { user :: String
                                 , request :: String
                                 } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

data Token = Token { success :: Bool
                   , encryptedTicket :: String
                   , sessionKey :: String
                   , timeout :: Int
                   , server :: String
                   } deriving (Show, Generic, FromJSON, ToJSON, ToBSON, FromBSON)

deriving instance FromBSON String
deriving instance ToBSON   String
deriving instance FromBSON Bool
deriving instance ToBSON   Bool
deriving instance FromBSON Int
deriving instance ToBSON   Int

type AuthAPI = "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] Token
