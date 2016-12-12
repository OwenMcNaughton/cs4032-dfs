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
import Data.Time.Clock.POSIX        (getPOSIXTime)
import Data.Time.Clock              (UTCTime(..), getCurrentTime)
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

  warnLog "Starting dir server"

  withMongoDbConnection $ insertMany "SERVERS" [
      ["no" =: show "fs0", "host" =: show "172.17.0.7", "port" =: show "8085", "ready" := val False],
      ["no" =: show "fs1", "host" =: show "172.17.0.9", "port" =: show "8085", "ready" := val False],
      ["no" =: show "fs2", "host" =: show "172.17.0.11", "port" =: show "8085", "ready" := val False],
      ["no" =: show "fs3", "host" =: show "172.17.0.13", "port" =: show "8085", "ready" := val False]]

  let settings = setPort (read dirPort :: Int) $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api Lib.server

api :: Proxy DirAPI
api = Proxy

server :: Server DirAPI
server = stat :<|> fsRegister

  where
    stat :: StatRequest -> Handler StatResponse
    stat stq@(StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout) = liftIO $ do
      if (xcrypt encStqTicket authServerSecret) == expectedTicket
        then do
          let sessionKey = xcrypt encStqSessionKey authServerSecret
          let user = xcrypt encStqUser sessionKey
          currentTime <- getPOSIXTime
          if stqTimeout > (round currentTime)
            then do
              let fullPath = xcrypt encFullPath sessionKey
              fileInfo <- (withMongoDbConnection $ findOne $ select ["fsFullPath" =: fullPath] "FILES")
              warnLog $ show fileInfo
              case fileInfo of
                Nothing -> do
                  let newFileID = myHash fullPath
                  let nfServerNo = "fs" ++ (show (myHashMod fullPath 4))
                  nfServer <- (withMongoDbConnection $ findOne $ select ["no" := val (show nfServerNo)] "SERVERS")
                  case nfServer of
                    Nothing -> do
                      warnLog "uh oh"
                      return $ StatResponse "" "" "" "" ""
                    Just nfs -> do
                      let newFile = FileStat fullPath (show newFileID) nfServerNo (getMongoString "host" nfs) (getMongoString "port" nfs) user True
                      withMongoDbConnection $ insert "FILES" (toBSON newFile)
                      let encFileId = xcrypt (show newFileID) authServerSecret
                      let encHost = xcrypt (getMongoString "host" nfs) sessionKey
                      let encPort = xcrypt (getMongoString "port" nfs) sessionKey
                      return $ StatResponse "Success! File created" encFullPath encHost encPort encFileId
                Just f -> do
                  let host' = xcrypt (getMongoString "fileServerHost" f) sessionKey
                  let port' = xcrypt (getMongoString "fileServerPort" f) sessionKey
                  let fileID' = xcrypt (getMongoString "fileID" f) authServerSecret
                  return $ StatResponse "Success! File exists" encFullPath host' port' fileID'
            else do
              warnLog "But his ticket timed out..."
              return $ StatResponse "Failure timeout" "" "" "" ""
        else do
          return $ StatResponse "Failure authentication" "" "" "" ""

    fsRegister :: String -> Handler Int
    fsRegister str = liftIO $ do
      -- TODO log this fs as still being alive...
      return 3
