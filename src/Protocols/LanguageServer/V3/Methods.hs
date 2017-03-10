{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Collection of methods that make up the language-server protocol, as 
-- specified in 
-- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md
module Protocols.LanguageServer.V3.Methods where

import Control.Monad.IO.Class
import Network.JsonRpc.Server

import Protocols.LanguageServer.V3.Data

methods :: (Monad m, MonadIO m) => [Method m]
methods = [initialize]

initialize :: (MonadIO m, Monad m) => Method m
initialize = toMethod "initialize" f (Required "InitializeParams" :+: ()) where
  f :: forall m. (MonadIO m, Monad m) => InitializeParams -> RpcResult m ()
  f i = liftIO $ print i