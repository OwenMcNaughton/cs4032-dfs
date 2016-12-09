{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

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

  let settings = setPort (read dirPort :: Int) $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api Lib.server

api :: Proxy DirAPI
api = Proxy

server :: Server DirAPI
server = download

  where
    download :: DownloadRequest -> Handler DownloadResponse
    download dlq@(DownloadRequest encUser secretTicket encSessionKey encFullPath timeout2) = liftIO $ do
      if (xcrypt secretTicket authServerSecret) == expectedTicket
        then do
          let sessionKey = xcrypt encSessionKey authServerSecret
          let decUser = xcrypt encUser sessionKey
          let decFullPath = xcrypt encFullPath sessionKey
          warnLog (decUser ++ " has authorized access to " ++ decFullPath)
          currentTime <- getPOSIXTime
          warnLog ("timeout: " ++ show timeout2)
          warnLog ("current: " ++ show (round currentTime))
          if timeout2 > (round currentTime)
            then do
              warnLog "And his ticket's still valid"
              return $ DownloadResponse "Success!" "file contents"
            else do
              warnLog "But his ticket timed out..."
              return $ DownloadResponse "Failure timeout" "file contents"
        else do
          return $ DownloadResponse "Failure authentication" "?"
