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
    ( someFunc
    ) where

import           Control.Monad                      (join, when)
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import Data.Map(Map, keys, fromList, (!))
import           Git.Embed
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Options.Applicative
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC
import           System.Console.ANSI
import           System.Environment
import           UseHaskellAPI
import           UseHaskellAPIClient
import           Common
import           Database.MongoDB
import           Data.Bson.Generic
import Network.HTTP.Types.Status
import Network.Wai.Logger
import Network.Wai

redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]

reportExceptionOr act b pass =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b'' pass

class ProcessResponse a where
  processResponse :: a -> String -> IO ()

instance ProcessResponse Token where
  processResponse r p = do
    warnLog $ show r
    let encTicket = (xcrypt (ticket r) p)
    let key = (xcrypt (encTokenSessionKey r) p)
    let encKey = (xcrypt (encEncSessionKey r) p)
    let t = Token True encTicket key encKey (tokenTimeout r) (tokenServerHost r) (tokenServerPort r)
    withMongoDbConnection $ Database.MongoDB.delete (select [] "TOKEN")
    withMongoDbConnection $ upsert (select [] "TOKEN") (toBSON t)

instance ProcessResponse DownloadResponse where
  processResponse r p = do
    putStrLn $ show r

instance ProcessResponse StatResponse where
  processResponse r p = do
    warnLog $ show r
    let host = xcrypt (encStrServerHost r) p
    let port = xcrypt (encStrServerPort r) p
    warnLog host
    warnLog port
    let fileID = encStrID r
    let fullPath = xcrypt (encFullPath r) p
    let cfs = ClientFileStat fullPath fileID host port
    withMongoDbConnection $ Database.MongoDB.delete (select ["encFileID" := val fullPath] "FILES")
    withMongoDbConnection $ upsert (select ["encFileID" := val fullPath] "FILES") (toBSON cfs)

doCall pass f h p = do
  let reply = (SC.runClientM f =<< env h p)
  reportExceptionOr processResponse reply pass

doLogin :: IO ()
doLogin = do
  user <- (withMongoDbConnection $ findOne (select [] "USER"))
  case user of
    Nothing -> do
      putStrLn "Provide a username and password as cmd line args"
    Just u -> do
      let name = (getMongoString "name" u)
      let pass = (getMongoString "pass" u)
      let h = authHost
      let p = authPort
      doCall pass (login (LoginRequest name (xcrypt loginRequestMessage pass))) h p

doStat :: String -> IO ()
doStat fn = do
  user <- (withMongoDbConnection $ findOne (select [] "USER"))
  case user of
    Nothing -> do
      putStrLn "Provide a username and password as cmd line args, and then login"
    Just u -> do
      let name = (getMongoString "name" u)
      token <- (withMongoDbConnection $ findOne (select [] "TOKEN"))
      case token of
        Nothing -> do
          warnLog "Didn't find an access token, did you login?"
        Just t -> do
          let h = (getMongoString "tokenServerHost" t)
          let p = (getMongoString "tokenServerPort" t)
          let encStqTicket = (getMongoString "ticket" t)
          let encStqSessionKey = (getMongoString "encEncSessionKey" t)
          let stqTimeout = (getMongoInt "tokenTimeout" t)
          let sessionKey = (getMongoString "encTokenSessionKey" t)
          let encStqUser = xcrypt name sessionKey
          let encFullPath = xcrypt fn sessionKey
          doCall sessionKey (stat (StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout)) h p

doDownload :: String -> IO ()
doDownload fn = do
  token <- (withMongoDbConnection $ findOne (select [] "TOKEN"))
  case token of
    Nothing -> do
      warnLog "Didn't find an access token, did you login?"
    Just t -> do
      fileInfo <- withMongoDbConnection $ findOne (select ["cfsFullPath" := val fn] "FILES")
      case fileInfo of
        Nothing -> do
          warnLog "I don't know that file..."
        Just fi -> do
          let timeout = (getMongoInt "tokenTimeout" t)
          let encDlqSessionKey = (getMongoString "encEncSessionKey" t)
          let sessionKey = (getMongoString "encTokenSessionKey" t)
          let fileID = show (getMongoString "encFileID" fi)
          let h = show (getMongoString "host" fi)
          let p = show (getMongoString "port" fi)
          doCall sessionKey (download (DownloadRequest fileID encDlqSessionKey timeout)) h p

setupUser :: IO ()
setupUser = do
  args <- getArgs
  let user = User (head (drop 0 args)) (head (drop 1 args))
  withMongoDbConnection $ Database.MongoDB.delete (select [] "USER")
  withMongoDbConnection $ upsert (select [] "USER") (toBSON user)

prompt :: IO ()
prompt = do
  setupUser
  putStrLn "Ready."
  line <- getLine
  if isPrefixOf "login" line
    then do
      doLogin
  else if isPrefixOf "stat" line
    then do
      let command = splitOn " " line
      doStat (head (drop 1 command))
  else if isPrefixOf "download" line
    then do
      let command = splitOn " " line
      doDownload (head (drop 1 command))
  else
    putStrLn "add"
  prompt

someFunc :: IO ()
someFunc = do
  setEnv "MONGODB_IP" "localhost"
  setEnv "MONGODB_DATABASE" "USEHASKELLDB"

  let reply = (SC.runClientM fsRegister =<< env dirHost dirPort)
  reportExceptionOr' processResponse' reply

  prompt





reportExceptionOr' act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

class ProcessResponse' a where
 processResponse' :: a -> IO ()

instance ProcessResponse' Int where
  processResponse' r = do
    warnLog $ show r
