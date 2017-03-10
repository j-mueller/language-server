module Main where

import Data.Semigroup
import Network.Socket hiding (recv)
import Network.Socket.ByteString.Lazy

import Protocols.LanguageServer.V3 (run)

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 1
  (conn, _) <- accept sock
  mainLoop conn
  close conn
  close sock

mainLoop :: Socket -> IO ()
mainLoop conn = do
  msg <- recv conn 1024
  response <- run msg
  sendAll conn (maybe mempty id response) >> mainLoop conn
