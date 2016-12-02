{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Control.Monad.Trans.Resource
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import           Data.Char                    (toUpper)
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (pack, unpack)
import           Data.Time.Clock              (UTCTime, getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Logger
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           System.Environment           (getArgs, getProgName, lookupEnv, setEnv)
import           System.Log.Formatter
import           System.Log.Handler           (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           UseHaskellAPI

startApp :: IO ()
startApp = withLogging $ \ aplogger -> do

  warnLog "Starting use-haskell."

  let settings = setPort 8080 $ setLogger aplogger defaultSettings
  runSettings settings app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = echo
    :<|> storeMessage
    :<|> searchMessage
    :<|> performRESTCall

  where
    echo :: Maybe String -> Handler ResponseData
    echo ms = liftIO $ do
      warnLog $ "echo request " ++ show ms
      case ms of
        Nothing -> do
          return $ ResponseData $ "you've gotta send a message"
        Just s  -> liftIO $ do
          return $ ResponseData $ (map toUpper s)

    storeMessage :: Message -> Handler Bool
    storeMessage msg@(Message key _) = liftIO $ do
      warnLog $ "Storing message under key " ++ key ++ "."
      withMongoDbConnection $ upsert (select ["name" =: key] "MESSAGE_RECORD") $ toBSON msg

      return True

    searchMessage :: Maybe String -> Handler [Message]
    searchMessage (Just key) = liftIO $ do
      warnLog $ "Searching for value for key: " ++ key

      withMongoDbConnection $ do
        docs <- find (select ["name" =: key] "MESSAGE_RECORD") >>= drainCursor
        --warnLog $ "retrieved data: " ++ show docs
        return $ catMaybes $ DL.map (\ b -> fromBSON b :: Maybe Message) docs

    searchMessage Nothing = liftIO $ do
      warnLog $ "No key for searching."
      return $ ([] :: [Message])

    performRESTCall :: Maybe String -> Handler ResponseData
    performRESTCall (Just filt) = liftIO $ do
      warnLog $ "recieved request to perform REST call with param " ++ filt
      doRest $ DL.filter (DL.isInfixOf filt)

    performRESTCall Nothing = liftIO $ do
      warnLog $ "recieved request to perform REST call, but no param "
      doRest id

    doRest :: ([String] -> [String]) -> IO ResponseData
    doRest flt = do
      res <- SC.runClientM getPackages =<< env
      case res of
        Left err -> do
          warnLog $ "Rest call failed with error: " ++ show err
          return $ ResponseData $ "Rest call failed with error: " ++ show err
        Right pkgs -> do
          return $ ResponseData $ DL.intercalate ", " $
                                  flt $
                                  DL.map (unpack . RestClient.packageName) pkgs
      where env = do
             manager <- newManager defaultManagerSettings
             return (SC.ClientEnv manager (SC.BaseUrl SC.Http "hackage.haskell.org" 80 ""))





-- Helper functions
custom404Error msg = err404 { errBody = msg }

iso8601 :: UTCTime -> String
iso8601 = formatTime defaultTimeLocale "%FT%T%q%z"

debugLog, warnLog, errorLog :: String -> IO ()
debugLog = doLog debugM
warnLog  = doLog warningM
errorLog = doLog errorM
noticeLog = doLog noticeM

doLog f s = getProgName >>= \ p -> do
                t <- getCurrentTime
                f p $ (iso8601 t) ++ " " ++ s

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger

withMongoDbConnection :: Action IO a -> IO a
withMongoDbConnection act  = do
  ip <- mongoDbIp
  port <- mongoDbPort
  database <- mongoDbDatabase
  pipe <- connect (host ip)
  ret <- runResourceT $ liftIO $ access pipe master (pack database) act
  close pipe
  return ret

drainCursor :: Cursor -> Action IO [Document]
drainCursor cur = drainCursor' cur []
  where
    drainCursor' cur res  = do
      batch <- nextBatch cur
      if null batch
        then return res
        else drainCursor' cur (res ++ batch)

mongoDbIp :: IO String
mongoDbIp = defEnv "MONGODB_IP" id "database" True

mongoDbPort :: IO Integer
mongoDbPort = defEnv "MONGODB_PORT" read 27017 False

mongoDbDatabase :: IO String
mongoDbDatabase = defEnv "MONGODB_DATABASE" id "USEHASKELLDB" True

logLevel :: IO String
logLevel = defEnv "LOG_LEVEL" id "DEBUG" True

defEnv :: Show a
              => String
              -> (String -> a)
              -> a
              -> Bool
              -> IO a
defEnv env fn def doWarn = lookupEnv env >>= \ e -> case e of
      Just s  -> return $ fn s
      Nothing -> do
        when doWarn (doLog warningM $ "Environment variable: " ++ env ++
                                      " is not set. Defaulting to " ++ (show def))
        return def
