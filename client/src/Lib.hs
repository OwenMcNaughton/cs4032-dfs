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

redCode   = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Red]
whiteCode = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid White]
blueCode  = setSGRCode [SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Blue]
resetCode = setSGRCode [Reset]

reportExceptionOr act b pass =  b >>= \ b' ->
  case b' of
     Left err -> putStrLn $ "Call failed with error: " ++ show err
     Right b'' ->  act b'' pass

class ProcessResponse a where
  processResponse :: a -> String -> IO()

instance ProcessResponse Token where
  processResponse r p = do
    putStrLn $ show r
    putStrLn ("SessionKey: " ++ (xcrypt (sessionKey r) p))
    putStrLn ("Encrypted-ticket: " ++ (xcrypt (encryptedTicket r) p))

instance ProcessResponse Bool where
  processResponse r p = do
    putStrLn "tytyt!"

instance ProcessResponse ResponseData where
  processResponse r p = do
    putStrLn "echo!"

instance ProcessResponse [Message] where
  processResponse r p = do
    putStrLn "adsdsa!"

doCall pass f h p = do
  let reply = (SC.runClientM f =<< env h p)
  reportExceptionOr processResponse reply pass

doLogin :: String -> String -> Maybe String -> Maybe String -> IO ()
doLogin u p = doCall p (login (LoginRequest u (xcrypt loginRequestMessage p)))

someFunc :: IO ()
someFunc = do
  join $ execParser =<< opts

opts :: IO (ParserInfo (IO ()))
opts = do
  progName <- getProgName

  return $ info (   helper
                <*> subparser
                       ( command "login"
                                   (withInfo ( doLogin
                                           <$> argument str (metavar "username")
                                           <*> argument str (metavar "password")
                                           <*> serverIpOption
                                           <*> serverPortOption) "login to the auth server" )))
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
