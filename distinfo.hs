{-

This file is part of the Haskell package distinfo. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/distinfo/LICENSE. No part
of the distinfo package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Control.Applicative
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import DistInfo
import Options.Applicative

--------------------------------------------------------------------------------
-- | Configuration from the command line.
data Config = Config
  { nodePort  :: String
  , nodeName  :: String
  , webPort   :: Int
  , webEnable :: Bool
  }

--------------------------------------------------------------------------------
-- | A parser that can create a 'Config' from the command line.
config :: Parser Config
config = Config <$> nport
                <*> name
                <*> wport
                <*> wenable
  where
    nport :: Parser String
    nport = strOption (long "node-port" <>
                       short 'n'        <>
                       metavar "PORT"   <>
                       value "4000"     <>
                       help "Port number for distributed-process")

    name :: Parser String
    name = strOption (long "hostname"   <>
                      short 'H'         <>
                      metavar "IP"      <>
                      value "localhost" <>
                      help "Public IP address")

    wport :: Parser Int
    wport = option (long "web-port" <>
                    short 'W'       <>
                    metavar "PORT"  <>
                    value 8000      <>
                    help "Web server port number")

    wenable :: Parser Bool
    wenable = flag False True (long "enable-web" <>
                               short 'w'         <>
                               help "Enable the web server")

--------------------------------------------------------------------------------
-- | Parse the command line and return a 'Config'.
commandLineOptions :: IO Config
commandLineOptions = execParser $ info options desc
  where options = helper <*> config
        desc    = progDesc "Stupid distributed-process app"

--------------------------------------------------------------------------------
-- | Discover peers and then start the server.
boot :: Config -> Backend -> Process ()
boot Config{..} backend = do
  nid   <- getSelfNode
  pid   <- getSelfPid
  peers <- filter (/= nid) <$> liftIO (findPeers backend 1000000)

  say $ "peers: " ++ show peers
  register "distInfoServer" pid

  mapM_ (`whereisRemoteAsync` "distInfoServer") peers
  distInfoServer $ if webEnable then Just webPort else Nothing

--------------------------------------------------------------------------------
main :: IO ()
main = do
  cfg@Config{..} <- commandLineOptions
  backend        <- initializeBackend nodeName nodePort initRemoteTable
  node           <- newLocalNode backend
  runProcess node $ boot cfg backend
