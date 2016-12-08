{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Lib
    ( startApp
    ) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import UseHaskellAPI
import Common
import Database.MongoDB
import Data.Bson.Generic

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting auth server"

  withMongoDbConnection $ insertMany "USERS" [
      ["name" =: show "owen", "pass" =: show "qwerty"],
      ["name" =: show "paul", "pass" =: show "1234"]]

  let settings = setPort 8081 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy AuthAPI
api = Proxy

server :: Server AuthAPI
server = login

  where
    login :: LoginRequest -> Handler ResponseData
    login lrq@(LoginRequest user req) = liftIO $ do
      warnLog $ "login request for " ++ show user
      pass <- (withMongoDbConnection $ findOne $ select ["name" =: show user] "USERS")
      case pass of
        Nothing -> do
          return $ ResponseData $ "user not found"
        Just p -> do
          let decrypted_msg = xcrypt req (trimPass (getMongoString "pass" p))
          warnLog decrypted_msg
          case decrypted_msg == loginRequestMessage of
            True -> do
              return $ ResponseData $ "you're logged in!"
            False ->
              return $ ResponseData $ "that's not the right password"
