module Protocols.LanguageServer.V3(
  run,
  -- * Default API (no capabilities)
  languageServerAPI
) where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy
import Network.JsonRpc.Server
import Protocols.LanguageServer.V3.Methods (
  LanguageServerAPI,
  languageServerAPI,
  generateMethods)

run :: (Monad m, FromJSON a) => LanguageServerAPI m a -> ByteString -> m (Maybe ByteString)
run  = call . generateMethods
