{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module UseHaskellAPIClient where

import           Data.Proxy
import           Servant.API
import           Servant.Client
import           UseHaskellAPI

authRestAPI :: Proxy AuthAPI
authRestAPI = Proxy

login :: LoginRequest -> ClientM Token

(login) = client authRestAPI


dirRestAPI :: Proxy DirAPI
dirRestAPI = Proxy

stat :: StatRequest -> ClientM StatResponse
fsRegister :: String -> ClientM Int
lock :: StatRequest -> ClientM StatResponse
unlock :: StatRequest -> ClientM StatResponse

(stat :<|> fsRegister :<|> lock :<|> unlock) = client dirRestAPI


fsRestAPI :: Proxy FsAPI
fsRestAPI = Proxy

download :: DownloadRequest -> ClientM DownloadResponse
upload :: UploadRequest -> ClientM UploadResponse
touch :: UploadRequest -> ClientM Int
fslock :: StatRequest -> ClientM Int
fsunlock :: StatRequest -> ClientM Int

(download :<|> upload :<|> touch :<|> fslock :<|> fsunlock) = client fsRestAPI
