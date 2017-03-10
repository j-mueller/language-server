module Protocols.LanguageServer.V3 where

import Data.ByteString.Lazy
import Network.JsonRpc.Server
import Protocols.LanguageServer.V3.Methods (methods)

run :: ByteString -> IO (Maybe ByteString)
run = call methods
