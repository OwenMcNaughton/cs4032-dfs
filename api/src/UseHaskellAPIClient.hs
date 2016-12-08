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


restAPI :: Proxy API
restAPI = Proxy

echo :: Maybe String -> ClientM ResponseData
storeMessage :: Message -> ClientM Bool
searchMessage :: Maybe String -> ClientM [Message]
performRestCall :: Maybe String -> ClientM ResponseData

(echo :<|> storeMessage :<|> searchMessage :<|> performRestCall) = client restAPI


authRestAPI :: Proxy AuthAPI
authRestAPI = Proxy

login :: LoginRequest -> ClientM ResponseData

(login) = client authRestAPI
