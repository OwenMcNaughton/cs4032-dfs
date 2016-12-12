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

reportExceptionOr act b aplogger =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b'' aplogger

class ProcessResponse a where
 processResponse :: a -> ApacheLogger -> IO ()

instance ProcessResponse Int where
  processResponse r aplogger = do
    warnLog $ show r
    let settings = setPort r $ setLogger aplogger defaultSettings
    runSettings settings app

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting fs server"

  let reply = (SC.runClientM fsRegister =<< env dirHost dirPort)
  reportExceptionOr processResponse reply aplogger

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
          contents <- getDirectoryContents "."
          warnLog (show contents)
          return $ DownloadResponse "" ""
        else do
          warnLog "But his ticket timed out..."
          return $ DownloadResponse "Failure timeout" ""

    upload :: UploadRequest -> Handler UploadResponse
    upload ulq@(UploadRequest encUlqID encUlqSessionkey ulqTimeout) = liftIO $ do
      return $ UploadResponse ""
