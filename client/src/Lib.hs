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
    putStrLn $ show r
    let ticket = (xcrypt (encryptedTicket r) p)
    let key = (xcrypt (sessionKey r) p)
    putStrLn ("SessionKey: " ++ key)
    putStrLn ("Encrypted-ticket: " ++ ticket)
    let t = Token True ticket key (timeout r) (server r)
    withMongoDbConnection $ Database.MongoDB.delete (select [] "DB")
    withMongoDbConnection $ upsert (select [] "DB") (toBSON t)

doCall pass f h n = do
  let reply = (SC.runClientM f =<< env h n)
  reportExceptionOr processResponse reply pass

doLogin :: String -> String -> Maybe String -> Maybe String -> IO ()
doLogin u p = doCall p (login (LoginRequest u (xcrypt loginRequestMessage p)))

prompt :: IO ()
prompt = do
  args <- getArgs
  let host = (head (drop 0 args))
  let port = (head (drop 1 args))
  putStrLn "Ready."
  line <- getLine
  if isPrefixOf "login" line
    then do
      let command = splitOn " " line
      doLogin (head (drop 1 command)) (head (drop 2 command)) (Just host) (Just port)
  else if isPrefixOf "download" line
    then do
      token <- (withMongoDbConnection $ findOne (select [] "DB"))
      case token of
        Nothing ->
          putStrLn "you need to login first"
        Just t -> do
          putStrLn (getMongoString "server" t)
  else
    putStrLn "noono"
  prompt

someFunc :: IO ()
someFunc = do
  setEnv "MONGODB_IP" "localhost"
  prompt

env :: Maybe String -> Maybe String -> IO SC.ClientEnv
env host port = SC.ClientEnv <$> newManager defaultManagerSettings
                                               <*> (SC.BaseUrl <$> pure SC.Http
                                                               <*> (host <?> usehaskellHost)
                                                               <*> (read <$> (port <?> usehaskellPort))
                                                               <*> pure "")
 where
   (<?>) :: Maybe a -> IO a -> IO a
   h <?> f = case h of
     Just hst -> return hst
     Nothing  -> f

   usehaskellHost :: IO String
   usehaskellHost = devEnv "USE_HASKELL_HOST" id "localhost" True

   usehaskellPort :: IO String
   usehaskellPort = devEnv "USE_HASKELL_PORT" id "8080" True

   devEnv :: Show a
          => String        -- Environment Variable name
          -> (String -> a)  -- function to process variable string (set as 'id' if not needed)
          -> a             -- default value to use if environment variable is not set
          -> Bool          -- True if we should warn if environment variable is not set
          -> IO a
   devEnv env fn def warn = lookupEnv env >>= \ result ->
     case result of
         Just s  -> return $ fn s
         Nothing -> warn' warn env def

    where warn' :: Show b => Bool -> String -> b -> IO b
          warn' wn e s =  do
            when wn $ putStrLn $ "Environment variable: " ++ e ++
                                    " is not set. Defaulting to " ++ (show s)
            return s
