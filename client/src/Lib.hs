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
    let decStatus = xcrypt (encDlrStatus r) p
    let decContents = xcrypt (encDlrContents r) p
    warnLog (decStatus ++ " " ++ decContents)

instance ProcessResponse UploadResponse where
  processResponse r p = do
    warnLog (xcrypt (encUlrStatus r) p)

instance ProcessResponse StatResponse where
  processResponse r p = do
    warnLog $ show r
    let host = trimPass (xcrypt (encStrServerHost r) p)
    let port = trimPass (xcrypt (encStrServerPort r) p)
    let fileID = encStrID r
    let fullPath = xcrypt (encFullPath r) p
    let cfs = ClientFileStat fullPath fileID host port
    withMongoDbConnection $ Database.MongoDB.delete (select ["cfsFullPath" := val fullPath] "FILES")
    withMongoDbConnection $ upsert (select ["cfsFullPath" := val fullPath] "FILES") (toBSON cfs)

doCall pass f h p = do
  let reply = (SC.runClientM f =<< env h p)
  reportExceptionOr processResponse reply pass

doLogin :: IO ()
doLogin = do
  user <- (withMongoDbConnection $ findOne (select [] "USER"))
  case user of
    Nothing -> do
      warnLog "Provide a username and password as cmd line args"
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

doLock :: String -> IO ()
doLock fn = do
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
          doCall sessionKey (lock (StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout)) h p

doUnlock :: String -> IO ()
doUnlock fn = do
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
          doCall sessionKey (unlock (StatRequest encStqUser encStqTicket encStqSessionKey encFullPath stqTimeout)) h p

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
          let fileID = (getMongoString "encFileID" fi)
          let h = getMongoString "host" fi
          let p = getMongoString "port" fi
          doCall sessionKey (download (DownloadRequest fileID encDlqSessionKey timeout)) h p

doUpload :: String -> String -> IO ()
doUpload fn contents = do
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
          fileInfo <- withMongoDbConnection $ findOne (select ["cfsFullPath" := val fn] "FILES")
          case fileInfo of
            Nothing -> do
              warnLog "I don't know that file..."
            Just fi -> do
              let timeout = (getMongoInt "tokenTimeout" t)
              let encDlqSessionKey = (getMongoString "encEncSessionKey" t)
              let sessionKey = (getMongoString "encTokenSessionKey" t)
              let fileID = (getMongoString "encFileID" fi)
              let h = getMongoString "host" fi
              let p = getMongoString "port" fi
              let encUser = xcrypt name sessionKey
              let encContents = xcrypt contents sessionKey
              doCall sessionKey (upload (UploadRequest fileID encUser encDlqSessionKey encContents timeout)) h p

setupUser :: IO ()
setupUser = do
  args <- getArgs
  let user = User (head (drop 0 args)) (head (drop 1 args))
  withMongoDbConnection $ Database.MongoDB.delete (select [] "USER")
  withMongoDbConnection $ upsert (select [] "USER") (toBSON user)

prompt :: IO ()
prompt = do
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
  else if isPrefixOf "upload" line
    then do
      let command = splitOn " " line
      doUpload (head (drop 1 command)) (head (drop 2 command))
  else if isPrefixOf "lock" line
    then do
      let command = splitOn " " line
      doLock (head (drop 1 command))
  else if isPrefixOf "unlock" line
    then do
      let command = splitOn " " line
      doUnlock (head (drop 1 command))
  else
    putStrLn "add"
  prompt

someFunc :: IO ()
someFunc = do
  setEnv "MONGODB_IP" "localhost"
  setEnv "MONGODB_DATABASE" "USEHASKELLDB"
  setupUser
  prompt
