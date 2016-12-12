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
  warnLog "Task scheduler operating."

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
server = download :<|> upload

  where
    download :: DownloadRequest -> Handler DownloadResponse
    download dlq@(DownloadRequest encDlqID encDlqSessionkey dlqTimeout) = liftIO $ do
      let sessionKey = xcrypt encDlqSessionkey authServerSecret
      currentTime <- getPOSIXTime
      if dlqTimeout > (round currentTime)
        then do
          let decID = xcrypt encDlqID authServerSecret
          contents <- readFile ("files/" ++ decID)
          warnLog contents
          return $ DownloadResponse "Success!" (xcrypt contents sessionKey)
        else do
          warnLog "But his ticket timed out..."
          return $ DownloadResponse "Failure timeout" ""

    upload :: UploadRequest -> Handler UploadResponse
    upload ulq@(UploadRequest encUlqID encUlqSessionkey ulqEncContents ulqTimeout) = liftIO $ do
      let sessionKey = xcrypt encUlqSessionkey authServerSecret
      currentTime <- getPOSIXTime
      if ulqTimeout > (round currentTime)
        then do
          let decID = xcrypt encUlqID authServerSecret
          let contents = xcrypt ulqEncContents sessionKey
          writeFile ("files/" ++ decID) contents
          return $ UploadResponse "Success!"
        else do
          warnLog "Ticket has timed out"
          return $ UploadResponse "Failure timeout"
      return $ UploadResponse "Success!"
