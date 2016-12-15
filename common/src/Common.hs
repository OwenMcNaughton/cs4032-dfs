{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Common where

import           Control.Monad                (when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Bits
import           Data.Char
import           Data.Hashable
import           Data.Time.Clock.POSIX
import           Data.Time.Clock              (UTCTime(..), getCurrentTime)
import           Data.Time.Format             (defaultTimeLocale, formatTime)
import           Data.Text                    (pack, unpack)
import           Network.HTTP.Client                (defaultManagerSettings,
                                                     newManager)
import           Data.UnixTime
import           Database.MongoDB
import           Network.Wai.Logger
import           System.Environment           (getArgs, getProgName, lookupEnv, setEnv)
import           System.Log.Logger
import qualified Servant.API                        as SC
import qualified Servant.Client                     as SC

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

withLogging act = withStdoutLogger $ \aplogger -> do

  lname  <- getProgName
  llevel <- logLevel
  updateGlobalLogger lname
                     (setLevel $ case llevel of
                                  "WARNING" -> WARNING
                                  "ERROR"   -> ERROR
                                  _         -> DEBUG)
  act aplogger

-- 'encrypts' a msg with a key. works in both directions.
xcrypt :: String -> String -> String
xcrypt msg key = zipWith (\a b -> chr $ xor (ord a) (ord b)) (cycle key) msg

loginRequestMessage :: String
loginRequestMessage = "Can I log in please?"

authServerSecret :: String
authServerSecret = "Only the auth server and the directory server know this"

expectedTicket :: String
expectedTicket = "Hello, this is an authorized request"

-- Extract the string value of mongodb field
getMongoString :: Label -> Document -> String
getMongoString label = typed . (valueAt label)

-- Extract the string value of mongodb field
getMongoInt :: Label -> Document -> Int
getMongoInt label = typed . (valueAt label)

getMongoBool :: Label -> Document -> Bool
getMongoBool label = typed . (valueAt label)

-- Remove quotes from a string...
trimPass :: String -> String
trimPass pass = take ((length pass) - 2) $ drop 1 $ pass

authPort :: String
authPort = "8081"

dirPort :: String
dirPort = "8082"

authHost :: String
authHost = "172.17.0.5"

dirHost :: String
dirHost = "172.17.0.3"

myHash :: String -> Int
myHash s = hash s

myHashMod :: String -> Int -> Int
myHashMod s m = (hash s) `mod` m

fileServer :: Int -> (String, String)
fileServer 0 = ("172.17.0.7", "8085")
fileServer 1 = ("172.17.0.9", "8085")
fileServer 2 = ("172.17.0.11", "8085")
fileServer 3 = ("172.17.0.13", "8085")

env :: String -> String -> IO SC.ClientEnv
env host port = do
  manager <- newManager defaultManagerSettings
  return (SC.ClientEnv manager (SC.BaseUrl SC.Http host (read port :: Int) ""))
