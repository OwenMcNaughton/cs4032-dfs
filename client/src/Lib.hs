{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

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
    let key = (xcrypt (encSessionKey r) p)
    let encKey = (xcrypt (encEncSessionKey r) p)
    let t = Token True encTicket key encKey (timeout r) (serverHost r) (serverPort r)
    withMongoDbConnection $ Database.MongoDB.delete (select [] "TOKEN")
    withMongoDbConnection $ upsert (select [] "TOKEN") (toBSON t)

instance ProcessResponse DownloadResponse where
  processResponse r p = do
    putStrLn $ show r

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

doDownload :: String -> IO ()
doDownload fn = do
  user <- (withMongoDbConnection $ findOne (select [] "USER"))
  case user of
    Nothing -> do
      putStrLn "Provide a username and password as cmd line args, and then login"
    Just u -> do
      let name = (getMongoString "name" u)
      token <- (withMongoDbConnection $ findOne (select [] "TOKEN"))
      case token of
        Nothing -> do
          putStrLn "Didn't find an access token, did you login?"
        Just t -> do
          let h = (getMongoString "serverHost" t)
          let p = (getMongoString "serverPort" t)
          let ticket = (getMongoString "ticket" t)
          let encSessionKey = (getMongoString "encEncSessionKey" t)
          let timeout3 = (getMongoInt "timeout" t)
          let sessionKey = (getMongoString "encSessionKey" t)
          let encUser = xcrypt name sessionKey
          let encFullPath = xcrypt fn sessionKey
          doCall "" (download (DownloadRequest encUser ticket encSessionKey encFullPath timeout3)) h p

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
  else if isPrefixOf "download" line
    then do
      let command = splitOn " " line
      doDownload (head (drop 1 command))
  else
    putStrLn "noono"
  prompt

someFunc :: IO ()
someFunc = do
  setEnv "MONGODB_IP" "localhost"
  setEnv "MONGODB_DATABASE" "USEHASKELLDB"
  prompt

env :: String -> String -> IO SC.ClientEnv
env host port = do
  manager <- newManager defaultManagerSettings
  return (SC.ClientEnv manager (SC.BaseUrl SC.Http host (read port :: Int) ""))
