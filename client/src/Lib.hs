{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib
    ( someFunc
    ) where

import           Control.Monad                      (join, when)
import           Data.List
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

redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]


reportExceptionOr act b =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b''

class PrintResponse a where
  resp :: Show a => a -> String

instance PrintResponse ResponseData where
  resp r = "Response is a single value: " ++ response r

instance PrintResponse [Message] where
  resp [] = "No messages."
  resp [x] = "Response is a single message: " ++ message x
  resp rs = "Response is an array with messages: " ++ (intercalate ", " $ map message rs)

instance PrintResponse [ResponseData] where
  resp rs = "Response is an array with values: " ++ (intercalate ", " $ map response rs)

instance PrintResponse Bool where
  resp True =  "Response is a boolean : Totally!"
  resp False = "Response is a boolean : Like No Way!"

doCall f h p = reportExceptionOr (putStrLn . resp) (SC.runClientM f =<< env h p)

doEcho :: String -> Maybe String -> Maybe String -> IO ()
doEcho s = doCall $ echo $ Just s

doStoreMessage :: String -> String -> Maybe String -> Maybe String -> IO ()
doStoreMessage n m  = doCall $ storeMessage $ Message n m

doSearchMessage :: String -> Maybe String -> Maybe String -> IO ()
doSearchMessage s  = doCall $ searchMessage $ Just s

doPerformRestCall :: Maybe String -> Maybe String -> Maybe String -> IO ()
doPerformRestCall s  =  doCall $ performRestCall s

someFunc :: IO ()
someFunc = do
  join $ execParser =<< opts

opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName

  return $ info (   helper
                <*> subparser
                       (  command "echo"
                                  (withInfo ( doEcho
                                          <$> argument str (metavar "message")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Uppercase a message" )
                       <> command "store-message"
                                  (withInfo ( doStoreMessage
                                          <$> argument str (metavar "Name")
                                          <*> argument str (metavar "Message")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Store a message on the remote server." )
                       <> command "search-message"
                                  (withInfo ( doSearchMessage
                                          <$> argument str (metavar "Name")
                                          <*> serverIpOption
                                          <*> serverPortOption) "Search for messages on the remote server." )
                       <> command "rest-call"
                                   (withInfo ( doPerformRestCall
                                           <$> optional (strOption ( long "search"
                                                                  <> short 's'
                                                                  <> help "The search string for the hackage call."))
                                           <*> serverIpOption
                                           <*> serverPortOption) "Do a hackage rest call from the remote server." )))
             (  fullDesc
             <> progDesc (progName ++ " is a simple test client for the use-haskell service." ++
                          " Try " ++ whiteCode ++ progName ++ " --help " ++ resetCode ++ " for more information. To " ++
                          " see the details of any command, " ++  "try " ++ whiteCode ++ progName ++ " COMMAND --help" ++
                          resetCode ++ ". The application supports bash completion. To enable, " ++
                          "ensure you have bash-completion installed and enabled (see your OS for details), the " ++
                          whiteCode ++ progName ++ resetCode ++
                          " application in your PATH, and place the following in your ~/.bash_profile : " ++ whiteCode ++
                          "source < (" ++ progName ++ " --bash-completion-script `which " ++ progName ++ "`)" ++
                          resetCode ))

-- helpers to simplify the creation of command line options
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc


serverIpOption :: Parser (Maybe String)
serverIpOption = optional $ strOption ( long "ip"
                                     <> short 'i'
                                     <> metavar "IPADDRESS"
                                     <> help "the ip address of the use-haskell service.")

serverPortOption :: Parser (Maybe String)
serverPortOption = optional $ strOption (  long "port"
                                        <> short 'n'
                                        <> metavar "PORT_NUMBER"
                                        <> help "The port number of the use-haskell service.")

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
