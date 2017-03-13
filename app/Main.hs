{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe (fromMaybe)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import           Protocols.LanguageServer.V3 (languageServerAPI, run)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  contents <- B.getContents
  let process = run languageServerAPI
  forM_ (B.lines contents) $ \request -> do
    process request >>= B.putStrLn . fromMaybe ""
