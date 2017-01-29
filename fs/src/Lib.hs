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

import           Control.Concurrent           (forkIO, threadDelay)
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
import System.Directory
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import Network.HTTP.Types.Status
import Network.Wai.Logger
import Network.HostName
import System.Environment           (lookupEnv)
import Data.List.Split
import System.IO

reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

class ProcessResponse a where
 processResponse :: a -> IO ()

instance ProcessResponse Int where
  processResponse r = do
    warnLog $ show r

taskScheduler :: Int -> IO ()
taskScheduler delay = do
  fsid <- lookupEnv "fsid"
  case fsid of
    Nothing -> do
      warnLog "no fsid env?"
    Just e -> do
      let f = fsRegister e
      let reply = (SC.runClientM f =<< env "172.17.0.3" dirPort)
      reportExceptionOr processResponse reply

  threadDelay $ delay * 1000000
  taskScheduler delay

lockFile :: String -> String -> IO ()
lockFile fileID lockedBy = liftIO $ do
  dfe <- doesFileExist ("files/" ++ fileID)
  if dfe
    then do
      contents <- readFile ("files/" ++ fileID)
      let splitContents = splitOn "||" contents
      writeFile ("files/" ++ fileID) ("L||" ++ lockedBy ++ "||" ++ (splitContents !! 2))
    else do
      warnLog "Failed to lock because the file doesn't exist"

unlockFile :: String -> IO ()
unlockFile fileID = liftIO $ do
  dfe <- doesFileExist ("files/" ++ fileID)
  if dfe
    then do
      contents <- readFile ("files/" ++ fileID)
      let splitContents = splitOn "||" contents
      writeFile ("files/" ++ fileID) ("U||blank||" ++ (splitContents !! 2))
    else do
      warnLog "Failed to lock because the file doesn't exist"

userHasAccess :: String -> String -> IO (Bool)
userHasAccess fileID user = liftIO $ do
  dfe <- doesFileExist ("files/" ++ fileID)
  if dfe
    then do
      contents <- readFile ("files/" ++ fileID)
      let splitContents = splitOn "||" contents
      if (splitContents !! 0) == "U"
        then do
          return True
        else do
          if (splitContents !! 1) == user
            then do
              return True
            else do
              return False
    else do
      return False

extractContents :: String -> IO (String)
extractContents fileID = liftIO $ do
  contents <- readFile ("files/" ++ fileID)
  let splitContents = splitOn "||" contents
  return (splitContents !! 2)

writeContents :: String -> String -> String ->  IO (Bool)
writeContents fileID user newContents = liftIO $ do
  dfe <- doesFileExist ("files/" ++ fileID)
  if dfe
    then do
      hasAccess <- userHasAccess fileID user
      if hasAccess
        then do
          hdl <- openFile ("files/" ++ fileID) ReadMode
          contents <- hGetContents hdl
          hClose hdl
          let splitContents = splitOn "||" contents
          hdl <- openFile ("files/" ++ fileID) WriteMode
          hPutStr hdl ((splitContents !! 0) ++ "||" ++ user ++ "||" ++ newContents)
          return True
        else do
          warnLog "Can't write contents because user does not have access"
          return False
    else do
      writeFile ("files/" ++ fileID) ("U||" ++ user ++ "||" ++ newContents)
      return True

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting fs server"

  createDirectoryIfMissing False "./files"

  forkIO $ taskScheduler 5

  let settings = setPort 8085 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api Lib.server

api :: Proxy FsAPI
api = Proxy

server :: Server FsAPI
server = download :<|> upload :<|> fslock :<|> fsunlock

  where
    download :: DownloadRequest -> Handler DownloadResponse
    download dlq@(DownloadRequest encDlqID encDlqUser encDlqSessionkey dlqTimeout) = liftIO $ do
      let sessionKey = xcrypt encDlqSessionkey authServerSecret
      let decUser = xcrypt encDlqUser sessionKey
      currentTime <- getPOSIXTime
      if dlqTimeout > (round currentTime)
        then do
          let decID = xcrypt encDlqID authServerSecret
          hasAccess <- userHasAccess decID decUser
          if hasAccess
            then do
              contents <- extractContents decID
              return $ DownloadResponse (xcrypt "Success!" sessionKey) (xcrypt contents sessionKey)
            else do
              return $ DownloadResponse (xcrypt "Failure, it's locked!" sessionKey) ""
        else do
          warnLog "But his ticket timed out..."
          return $ DownloadResponse (xcrypt "Failure: Timeout" sessionKey) ""

    upload :: UploadRequest -> Handler UploadResponse
    upload ulq@(UploadRequest encUlqID encUlqUser encUlqSessionkey ulqEncContents ulqTimeout) = liftIO $ do
      let sessionKey = xcrypt encUlqSessionkey authServerSecret
      let decUser = xcrypt encUlqUser sessionKey
      currentTime <- getPOSIXTime
      if ulqTimeout > (round currentTime)
        then do
          let decID = xcrypt encUlqID authServerSecret
          let newContents = xcrypt ulqEncContents sessionKey
          success <- writeContents decID decUser newContents
          if success
            then do
              return $ UploadResponse (xcrypt "Success!" sessionKey)
            else do
              return $ UploadResponse (xcrypt "Failure, it's locked" sessionKey)
        else do
          return $ UploadResponse (xcrypt "Failure: Timeout" sessionKey)

    touch :: UploadRequest -> Handler Int
    touch tlq@(UploadRequest encUlqID encUlqUser encUlqSessionKey ulqEncContents ulqTimeout) = liftIO $ do
      currentTime <- getPOSIXTime
      if ulqTimeout > (round currentTime)
        then do
          let decID = xcrypt encUlqID authServerSecret
          return 1
        else do
          return 0

    fslock :: StatRequest -> Handler Int
    fslock stq@(StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout) = liftIO $ do
      let ticket = xcrypt encStqTicket authServerSecret
      let decUser = xcrypt encStqUser authServerSecret
      if ticket == expectedTicket
        then do
          let fileID = xcrypt encFullPath authServerSecret
          lockFile fileID decUser
          return 1
        else do
          return 0

    fsunlock :: StatRequest -> Handler Int
    fsunlock stq@(StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout) = liftIO $ do
      let ticket = xcrypt encStqTicket authServerSecret
      if ticket == expectedTicket
        then do
          let fileID = xcrypt encFullPath authServerSecret
          unlockFile fileID
          return 1
        else do
          return 0
