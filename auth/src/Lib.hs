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
import           Data.Time.Clock.POSIX        (getPOSIXTime)
import           Data.Time.Clock              (UTCTime(..), getCurrentTime)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import UseHaskellAPI
import Common
import Database.MongoDB
import Data.Bson.Generic
import System.Random

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting auth server"

  withMongoDbConnection $ insertMany "USERS" [
      ["name" =: show "owen", "pass" =: show "qwerty"],
      ["name" =: show "paul", "pass" =: show "1234"]]

  let settings = setPort 8081 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api Lib.server

api :: Proxy AuthAPI
api = Proxy

server :: Server AuthAPI
server = login

  where
    login :: LoginRequest -> Handler Token
    login lrq@(LoginRequest user req) = liftIO $ do
      warnLog $ "login request for " ++ show user
      pass <- (withMongoDbConnection $ findOne $ select ["name" =: show user] "USERS")
      case pass of
        Nothing -> do
          return $ Token False "" "" 0 ""
        Just p -> do
          let password = trimPass (getMongoString "pass" p)
          let decrypted_msg = xcrypt req password
          warnLog (req ++ "   decrypts to  " ++ decrypted_msg)
          case decrypted_msg == loginRequestMessage of
            True -> do
              sessionKey <- randomRIO (0, 100000 :: Int)
              let encryptedTicket = xcrypt (show sessionKey) authServerSecret
              let encryptedSessionKey = xcrypt (show sessionKey) password
              let doublyEncryptedTicket = xcrypt encryptedTicket password
              warnLog ("SessionKey: " ++ (show sessionKey))
              warnLog ("Ticket: " ++ (show encryptedTicket))
              time <- getPOSIXTime
              return $ Token True doublyEncryptedTicket encryptedSessionKey ((round time) + 3600) "server111"
            False ->
              return $ Token False "" "" 0 ""
