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
import UseHaskellAPIClient
import Common
import Database.MongoDB
import Data.Bson.Generic
import System.Random
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           Control.Concurrent           (forkIO, threadDelay)

reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

class ProcessResponse a where
 processResponse :: a -> IO ()

instance ProcessResponse Int where
  processResponse r = do
    warnLog $ show r

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

doLock :: String -> String -> IO ()
doLock fullPath user = do
  let fileID = myHash fullPath
  let nfServerNo = "fs" ++ (show (myHashMod fullPath 4))
  nfServer <- (withMongoDbConnection $ findOne $ select ["no" := val (show nfServerNo)] "SERVERS")
  case nfServer of
    Nothing -> do
      warnLog "uh oh"
    Just nfs -> do
      let h = trimPass (getMongoString "host" nfs)
      let p = trimPass (getMongoString "port" nfs)
      let encTick = xcrypt expectedTicket authServerSecret
      let encUser = xcrypt user authServerSecret
      let stq = StatRequest encUser encTick "" (xcrypt (show fileID) authServerSecret) 0
      let f = fslock stq
      let reply = (SC.runClientM f =<< env h p)
      reportExceptionOr processResponse reply

doUnlock :: String -> IO ()
doUnlock fullPath = do
  let fileID = myHash fullPath
  let nfServerNo = "fs" ++ (show (myHashMod fullPath 4))
  nfServer <- (withMongoDbConnection $ findOne $ select ["no" := val (show nfServerNo)] "SERVERS")
  case nfServer of
    Nothing -> do
      warnLog "uh oh"
    Just nfs -> do
      let h = trimPass (getMongoString "host" nfs)
      let p = trimPass (getMongoString "port" nfs)
      let encTick = xcrypt expectedTicket authServerSecret
      let stq = StatRequest "" encTick "" (xcrypt (show fileID) authServerSecret) 0
      let f = fsunlock stq
      let reply = (SC.runClientM f =<< env h p)
      reportExceptionOr processResponse reply

doTouch :: String -> IO ()
doTouch fullPath = do
  let fileID = myHash fullPath
  let nfServerNo = "fs" ++ (show (myHashMod fullPath 4))
  nfServer <- (withMongoDbConnection $ findOne $ select ["no" := val (show nfServerNo)] "SERVERS")
  case nfServer of
    Nothing -> do
      warnLog "uh oh"
    Just nfs -> do
      let h = trimPass (getMongoString "host" nfs)
      let p = trimPass (getMongoString "port" nfs)
      let ulq = UploadRequest (xcrypt (show fileID) authServerSecret) "" "" "" 0
      let f = touch ulq
      let reply = (SC.runClientM f =<< env h p)
      reportExceptionOr processResponse reply

app :: Application
app = serve api Lib.server

api :: Proxy DirAPI
api = Proxy

server :: Server DirAPI
server = stat :<|> fsRegister :<|> lock :<|> unlock

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
                      let h = (getMongoString "host" nfs)
                      let p = (getMongoString "port" nfs)
                      let newFile = FileStat fullPath (show newFileID) nfServerNo h p False ""
                      withMongoDbConnection $ insert "FILES" (toBSON newFile)
                      let encFileId = xcrypt (show newFileID) authServerSecret
                      let encHost = xcrypt (getMongoString "host" nfs) sessionKey
                      let encPort = xcrypt (getMongoString "port" nfs) sessionKey
                      doTouch fullPath
                      return $ StatResponse "Success! File created" encFullPath encHost encPort encFileId
                Just f -> do
                  let host' = xcrypt (getMongoString "fileServerHost" f) sessionKey
                  let port' = xcrypt (getMongoString "fileServerPort" f) sessionKey
                  let fileID' = xcrypt (getMongoString "fileID" f) authServerSecret
                  let locked = getMongoBool "locked" f
                  case locked of
                    True -> do
                      let lockedBy = getMongoString "lockedBy" f
                      let msg = ("Success! File exists and is locked by: " ++ lockedBy)
                      return $ StatResponse msg encFullPath host' port' fileID'
                    False -> do
                      let msg = "Success! File exists and not locked"
                      return $ StatResponse msg encFullPath host' port' fileID'
            else do
              return $ StatResponse "Failure timeout" "" "" "" ""
        else do
          return $ StatResponse "Failure authentication" "" "" "" ""

    fsRegister :: String -> Handler Int
    fsRegister str = liftIO $ do
      warnLog ("Heartbeat: " ++  str)
      -- TODO log this fs as still being alive...
      return 3

    lock :: StatRequest -> Handler StatResponse
    lock stq@(StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout) = liftIO $ do
      if (xcrypt encStqTicket authServerSecret) == expectedTicket
        then do
          let sessionKey = xcrypt encStqSessionKey authServerSecret
          let user = xcrypt encStqUser sessionKey
          currentTime <- getPOSIXTime
          if stqTimeout > (round currentTime)
            then do
              let fullPath = xcrypt encFullPath sessionKey
              fileInfo <- (withMongoDbConnection $ findOne $ select ["fsFullPath" =: fullPath] "FILES")
              case fileInfo of
                Nothing -> do
                  return $ StatResponse "File does not exist, was not created, was not locked" "" "" "" ""
                Just f -> do
                  let locked = getMongoBool "locked" f
                  let lockedBy = getMongoString "lockedBy" f
                  case locked of
                    True -> do
                      return $ StatResponse ("Failure, lock already held by " ++ lockedBy) "" "" "" ""
                    False -> do
                      withMongoDbConnection $ fetch (select ["fsFullPath" =: fullPath] "FILES")
                        >>= save "FILES" . merge ["locked" =: val True]
                      withMongoDbConnection $ fetch (select ["fsFullPath" =: fullPath] "FILES")
                        >>= save "FILES" . merge ["lockedBy" =: val user]
                      doLock fullPath user
                      return $ StatResponse "Success! File locked" "" "" "" ""
            else do
              return $ StatResponse "Failure timeout" "" "" "" ""
        else do
          return $ StatResponse "Failure authentication" "" "" "" ""

    unlock :: StatRequest -> Handler StatResponse
    unlock stq@(StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout) = liftIO $ do
      if (xcrypt encStqTicket authServerSecret) == expectedTicket
        then do
          let sessionKey = xcrypt encStqSessionKey authServerSecret
          let user = xcrypt encStqUser sessionKey
          currentTime <- getPOSIXTime
          if stqTimeout > (round currentTime)
            then do
              let fullPath = xcrypt encFullPath sessionKey
              fileInfo <- (withMongoDbConnection $ findOne $ select ["fsFullPath" =: fullPath] "FILES")
              case fileInfo of
                Nothing -> do
                  return $ StatResponse "Failure: File does not exist" "" "" "" ""
                Just f -> do
                  let locked = getMongoBool "locked" f
                  let lockedBy = getMongoString "lockedBy" f
                  case locked of
                    False -> do
                      return $ StatResponse "Success! File already unlocked" "" "" "" ""
                    True -> do
                      if lockedBy == user then do
                        withMongoDbConnection $ fetch (select ["fsFullPath" =: fullPath] "FILES")
                          >>= save "FILES" . merge ["locked" =: val False]
                        doUnlock fullPath
                        return $ StatResponse "Success! File unlocked" "" "" "" ""
                      else do
                        return $ StatResponse ("Failure: File locked by: " ++ lockedBy) "" "" "" ""
            else do
              return $ StatResponse "Failure timeout" "" "" "" ""
        else do
          return $ StatResponse "Failure authentication" "" "" "" ""
