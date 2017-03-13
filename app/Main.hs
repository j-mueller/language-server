{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe (fromMaybe)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import           Protocols.LanguageServer.V3 (languageServerAPI, run)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  responses <- newChan 
  requests <- newChan

  -- Start reading requests
  _ <- forkIO $ readRequests requests

  -- Start writing responses
  _ <- forkIO $ printResponses responses

  -- Start processing the incoming messages
  let server = run languageServerAPI
  processMessages server requests responses

-- | Read messages from a channel and process them each in its own thread
processMessages :: (B.ByteString -> IO (Maybe B.ByteString)) -> Chan B.ByteString -> Chan B.ByteString -> IO ()
processMessages server requests responses = go where
  go = do
    msg <- readChan requests
    _ <- forkIO $ server msg >>= maybe (return ()) (writeChan responses)
    go

-- | Write the responses back to stdio
printResponses :: Chan B.ByteString -> IO ()
printResponses ch = go where
  go = (readChan ch >>= B.putStrLn) >> go

-- | Read requests from stdio
readRequests :: Chan B.ByteString -> IO ()
readRequests ch = go where
  go = (B.getContents >>= writeChan ch) >> go