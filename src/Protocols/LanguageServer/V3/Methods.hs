{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
-- | Collection of methods that make up the language-server protocol, as 
-- specified in 
-- https://github.com/Microsoft/language-server-protocol/blob/master/protocol.md
module Protocols.LanguageServer.V3.Methods where

import Control.Monad.IO.Class
import Data.Aeson (FromJSON)
import Network.JsonRpc.Server

import Protocols.LanguageServer.V3.Data

data LanguageServerAPI m a = LanguageServerAPI {
  _initialize :: InitializeParams a -> RpcResult m InitializeResult
}

-- | Default (empty) `LanguageServerAPI`
languageServerAPI :: Monad m => LanguageServerAPI m ()
languageServerAPI = LanguageServerAPI init where
  init _ = return initializeResult

generateMethods :: (Monad m, FromJSON a) => LanguageServerAPI m a -> [Method m]
generateMethods api = [initialize] where
  initialize = toMethod "initialize" (_initialize api) (Required "InitializeParams" :+: ())
