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

download :: DownloadRequest -> ClientM DownloadResponse

(download) = client dirRestAPI
