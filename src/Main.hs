{-# LANGUAGE DeriveDataTypeable, RecordWildCards, ScopedTypeVariables #-} 
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

-- | Main entry point, just connect to the given IRC server and join
-- the given channels and log all messages received, to the given
-- file(s).

module Main where

import Data.List
import Control.Concurrent.Delay
import Control.Monad.Fix
import Network
import Network.IRC
import System.Console.CmdArgs
import System.IO
import System.Posix

-- | Options for the executable.
data Options = Options
  { host      :: String       -- ^ The IRC server host/IP.
  , port      :: Int          -- ^ The remote port to connect to.
  , channels  :: String       -- ^ The channels to join (comma or
                              -- space sep'd).
  , logpath   :: FilePath     -- ^ Which directory to log to.
  , pass      :: Maybe String -- ^ Maybe a *server* password.
  , nick      :: String       -- ^ The nick.
  , user      :: String       -- ^ User (not real name) to use.
  } deriving (Show,Data,Typeable)

-- | Options for the executable.
options :: Options
options  = Options
  { host = def &= opt "irc.freenode.net" &= help "The IRC server."
  , port = def &= opt (6667::Int) &= help "The remote port."
  , channels = def &= opt "#hog" &= help "The channels to join."
  , logpath = def &= opt "." &= help "The directory to save log files."
  , pass = Nothing
  , nick = def &= opt "hog" &= help "The nickname to use."
  , user = def &= opt "hog" &= help "The user name to use."
  }
  &= summary "Hog IRC logger (C) Chris Done 2011"
  &= help "Simple IRC logger bot."

-- | Main entry point.
main :: IO ()
main = withSocketsDo $ do
  _ <- installHandler sigPIPE Ignore Nothing
  cmdArgs options >>= start

-- | Connect to the IRC server and start logging.
start :: Options -> IO ()
start options@Options{..} = do
  hSetBuffering stdout NoBuffering
  h <- connectTo host (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  register h options
  fix $ \repeat -> do
    line <- catch (Just `fmap` hGetLine h) $ \_e -> do
      delaySeconds 30
      start options
      return Nothing
    flip (maybe (return ())) line $ \line -> do
       putStrLn $ "<- " ++ line
       handleLine options h line
       repeat

-- | Register to the server by sending user/nick/pass/etc.
register :: Handle -> Options -> IO ()
register h Options{..} = do
  let send = sendLine h
  maybe (return ()) (send . ("PASS "++)) pass
  send $ "USER " ++ user ++ " * * *"
  send $ "NICK " ++ nick

-- | Handle incoming lines; ping/pong, privmsg, etc.
handleLine :: Options -> Handle -> String -> IO ()
handleLine options handle line = 
  case decode line of
    Nothing -> putStrLn $ "Unable to decode line " ++ show line
    Just msg -> handleMsg options handle msg
    
-- | Handle an IRC message.
handleMsg :: Options -> Handle -> Message -> IO ()
handleMsg options h msg =
  case msg_command msg of
    "PING" -> reply $ msg {msg_command="PONG"}
    "376"  -> joinChannels options h
    _ -> return ()
    
  where reply = sendLine h . encode
  
-- | Join the requested channels.
joinChannels :: Options -> Handle -> IO ()
joinChannels Options{..} h = do
  let chans = words $ replace ',' ' ' channels
  putStrLn $ "Joining channels: " ++ show chans
  sendLine h $ "JOIN :" ++ intercalate "," chans

-- | Replace x with y in xs.
replace :: Eq a => a -> a -> [a] -> [a]
replace c y = go where
    go [] = []
    go (c':cs) | c'==c     = y : go cs
               | otherwise = c' : go cs

-- | Send a line on a handle, ignoring errors (like, if the socket's
-- closed.)
sendLine :: Handle -> String -> IO ()
sendLine h line = do
  catch (do hPutStrLn h line; putStrLn $ "-> " ++ line) $
    \_e -> return ()
