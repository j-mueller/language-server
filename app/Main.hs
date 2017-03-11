{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Maybe (fromMaybe)
import           System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

import           Protocols.LanguageServer.V3 (run)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  contents <- B.getContents
  forM_ (B.lines contents) $ \request -> do
    run request >>= B.putStrLn . fromMaybe ""
