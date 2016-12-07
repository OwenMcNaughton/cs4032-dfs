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

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except   (ExceptT)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Bson.Generic
import qualified Data.ByteString.Lazy         as L
import           Data.Char                    (toUpper)
import qualified Data.List                    as DL
import           Data.Maybe                   (catMaybes)
import           Data.Text                    (unpack)
import           Database.MongoDB
import           GHC.Generics
import           Network.HTTP.Client          (defaultManagerSettings,
                                               newManager)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           RestClient
import           Servant
import qualified Servant.API                  as SC
import qualified Servant.Client               as SC
import           UseHaskellAPI
import           Common

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
