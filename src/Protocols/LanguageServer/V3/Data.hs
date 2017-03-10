{-# LANGUAGE OverloadedStrings #-}
-- | Data types and instances for language server protocol V3
module Protocols.LanguageServer.V3.Data where

import Data.Aeson
import Data.Semigroup
import Data.Text (Text)
import qualified Data.Text as T

data TraceSetting = TraceOff | TraceMessages | TraceVerbose
  deriving (Eq, Ord, Show)

instance FromJSON TraceSetting where
  parseJSON (String v) = case v of
    "off" -> return TraceOff
    "messages" -> return TraceMessages
    "verbose" -> return TraceVerbose
    e -> fail $ "Unknown trace setting: " <> (T.unpack e)
  parseJSON e = fail $ "Expected string when parsing `TraceSetting`, got " <> (show e)

data WorkspaceClientCapabilities = WorkspaceClientCapabilities {
  _applyEdit :: Maybe Bool -- TODO: Remaining fields
} deriving (Eq, Ord, Show)

instance FromJSON WorkspaceClientCapabilities where
  parseJSON (Object o) = WorkspaceClientCapabilities <$> o .: "applyEdit"
  parseJSON e = fail $ "Expected object when parsing `WorkspaceClientCapabilities`, got " <> (show e)

data TextDocumentClientCapabilities = TextDocumentClientCapabilities ()
  deriving (Eq, Ord, Show) -- TODO: Implement

instance FromJSON TextDocumentClientCapabilities where
  parseJSON (Object o) = return $ TextDocumentClientCapabilities ()
  parseJSON e = fail $ "Expected object when parsing `TextDocumentClientCapabilities`, got " <> (show e)

data ClientCapabilities = ClientCapabilities {
  _workspace :: WorkspaceClientCapabilities,
  _textDocument :: TextDocumentClientCapabilities,
  _experimental :: Text
} deriving (Eq, Ord, Show)

instance FromJSON ClientCapabilities where
  parseJSON (Object o) = ClientCapabilities <$>
                         o .: "workspace" <*>
                         o .: "textDocument" <*>
                         o .: "experimental"
  parseJSON e = fail $ "Expected object when parsing `ClientCapabilities`, got " <> (show e)

type DocumentUri = Text

data InitializeParams = InitializeParams {
  _processId :: Maybe Integer,
  _rootPath  :: Maybe Text,
  _rootUri   :: Maybe Text, -- TODO: DocumentUri
  _initializationOptions :: Maybe (), -- TODO: Parameter, FromJSON
  _capabilities :: Text, -- TODO: ClientCapabilities
  _trace :: Maybe TraceSetting
}  deriving (Eq, Ord, Show)

instance FromJSON InitializeParams where
  parseJSON (Object v) = InitializeParams <$>
                         v .: "processId" <*>
                         v .: "rootPath"  <*>
                         v .: "rootUri"   <*>
                         v .: "initializationOptions" <*>
                         v .: "capabilities" <*>
                         v .: "trace"
  parseJSON e = fail $ "Expected object when parsing `InitializeParams`, got " <> (show e)