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

data DocumentChangesCapability = DocumentChangesCapability { 
  _documentChanges :: Maybe Bool
} deriving (Eq, Ord, Show)

instance FromJSON DocumentChangesCapability where
  parseJSON (Object o) = DocumentChangesCapability <$> o .: "documentChanges"
  parseJSON e = fail $ "Expected objec when parsing `DocumentChangesCapability`, got " <> (show e)

data DynamicRegistrationCapability = DynamicRegistrationCapability {
  _dynamicRegistration :: Maybe Bool
} deriving (Eq, Ord, Show)

instance FromJSON DynamicRegistrationCapability where
  parseJSON (Object o) = DynamicRegistrationCapability <$> o .: "dynamicRegistration"
  parseJSON e = fail $ "Expected object when parsing `DynamicRegistrationCapability`, got " <> (show e)

data WorkspaceClientCapabilities = WorkspaceClientCapabilities {
  _applyEdit :: Maybe Bool, 
  _workspaceEdit :: Maybe DocumentChangesCapability,
  _didChangeConfiguration :: Maybe DynamicRegistrationCapability,
  _didChangeWatchedFiles :: Maybe DynamicRegistrationCapability,
  _symbol :: Maybe DynamicRegistrationCapability,
  _executeCommand :: Maybe DynamicRegistrationCapability
} deriving (Eq, Ord, Show)

instance FromJSON WorkspaceClientCapabilities where
  parseJSON (Object o) = WorkspaceClientCapabilities <$> 
                          o .: "applyEdit" <*>
                          o .: "workspaceEdit" <*>
                          o .: "didChangeConfiguration" <*>
                          o .: "didChangeWatchedFiles" <*>
                          o .: "symbol" <*>
                          o .: "executeCommand"
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

type DocumentUri = Text -- TODO: DocumentUri

data InitializeParams a = InitializeParams {
  _processId :: Maybe Integer,
  _rootPath  :: Maybe Text,
  _rootUri   :: Maybe DocumentUri,
  _initializationOptions :: Maybe a, -- TODO: Parameter, FromJSON
  _clientCapabilities :: ClientCapabilities, -- TODO: ClientCapabilities
  _trace :: Maybe TraceSetting
}  deriving (Eq, Ord, Show)

instance FromJSON a => FromJSON (InitializeParams a) where
  parseJSON (Object v) = InitializeParams <$>
                         v .: "processId" <*>
                         v .: "rootPath"  <*>
                         v .: "rootUri"   <*>
                         v .: "initializationOptions" <*>
                         v .: "capabilities" <*>
                         v .: "trace"
  parseJSON e = fail $ "Expected object when parsing `InitializeParams`, got " <> (show e)

newtype SaveOptions = SaveOptions {
  _includeText :: Maybe Bool
} deriving (Eq, Ord, Show)

instance ToJSON SaveOptions where
  toJSON (SaveOptions a) = object ["includeText" .= a]

data TextDocumentSyncOptions = TextDocumentSyncOptions {
  _openClose :: Maybe Bool,
  _change :: Maybe Integer,
  _willSave :: Maybe Bool,
  _willSaveWaitUntil :: Maybe Bool,
  _save :: Maybe SaveOptions
} deriving (Eq, Ord, Show)

instance ToJSON TextDocumentSyncOptions where
  toJSON td = object [
    "openClose" .= _openClose td,
    "change" .= _change td,
    "willSave" .= _willSave td,
    "willSaveWaitUntil" .= _willSaveWaitUntil td,
    "save" .= _save td
    ]

data CompletionOptions = CompletionOptions {
  _resolveProvider :: Maybe Bool,
  _completionTriggerCharacters :: Maybe [Text]
} deriving (Eq, Ord, Show)

instance ToJSON CompletionOptions where
  toJSON co = object [
    "resolveProvider" .= _resolveProvider co,
    "triggerCharacters" .= _completionTriggerCharacters co
    ]

newtype SignatureHelpOptions = SignatureHelpOptions {
  _signatureHelpTriggerCharacters :: Maybe [Text]
} deriving (Eq, Ord, Show)

instance ToJSON SignatureHelpOptions where
  toJSON sh = object [ "triggerCharacters" .= _signatureHelpTriggerCharacters sh]

newtype CodeLensOptions = CodeLensOptions {
  _codeLensResolveProvider :: Maybe Bool
} deriving (Eq, Ord, Show)

instance ToJSON CodeLensOptions where
  toJSON clo = object ["resolveProvider" .= _codeLensResolveProvider clo]

newtype DocumentLinkOptions = DocumentLinkOptions {
  _documentLinkResolveProvider :: Maybe Bool
} deriving (Eq, Ord, Show)

instance ToJSON DocumentLinkOptions where
  toJSON dlo = object ["resolveProvider" .= _documentLinkResolveProvider dlo]

newtype ExecuteCommandOptions = ExecuteCommandOptions {
  _commands :: Maybe [Text]
} deriving (Eq, Ord, Show)

instance ToJSON ExecuteCommandOptions where
  toJSON eco = object [ "commands" .= _commands eco ]

data DocumentOnTypeFormattingOptions = DocumentOnTypeFormattingOptions {
  _firstTriggerCharacter :: Text,
  _moreTriggerCharacters :: Maybe [Text]
} deriving (Eq, Ord, Show)

instance ToJSON DocumentOnTypeFormattingOptions where 
  toJSON dotfo = object [
    "firstTriggerCharacter" .= _firstTriggerCharacter dotfo,
    "moreTriggerCharacter"  .= _moreTriggerCharacters dotfo
    ]

data ServerCapabilities = ServerCapabilities {
  _textDocumentSync :: Maybe TextDocumentSyncOptions,
  _hoverProvider :: Maybe Bool,
  _completionProvider :: Maybe CompletionOptions,
  _signatureHelpProvider :: Maybe SignatureHelpOptions,
  _definitionProvider :: Maybe Bool,
  _referencesProvider :: Maybe Bool,
  _documentHighlightProvider :: Maybe Bool,
  _documentSymbolProvider :: Maybe Bool,
  _workspaceSymbolProvider :: Maybe Bool,
  _codeActionProvider :: Maybe Bool,
  _codeLensProvider :: Maybe CodeLensOptions,
  _documentFormattingProvider :: Maybe Bool,
  _documentRangeFormattingProvider :: Maybe Bool,
  _documentOnTypeFormattingProvider :: Maybe DocumentOnTypeFormattingOptions,
  _renameProvider :: Maybe Bool,
  _documentLinkProvider :: Maybe DocumentLinkOptions,
  _executeCommandProvider :: Maybe ExecuteCommandOptions
  -- experimental :: any
} deriving (Eq, Ord, Show)

instance ToJSON ServerCapabilities where
  toJSON sc = object [
    "textDocumentSync" .= _textDocumentSync sc,
    "hoverProvider" .= _hoverProvider sc,
    "completionProvider" .= _completionProvider sc,
    "signatureHelpProvider" .= _signatureHelpProvider sc,
    "definitionProvider" .= _definitionProvider sc,
    "referencesProvider" .= _referencesProvider sc,
    "documentHighlightProvider" .= _documentHighlightProvider sc,
    "documentSymbolProvider" .= _documentSymbolProvider sc,
    "workspaceSymbolProvider" .= _workspaceSymbolProvider sc,
    "codeActionProvider" .= _codeActionProvider sc,
    "codeLensProvider" .= _codeLensProvider sc,
    "documentFormattingProvider" .= _documentFormattingProvider sc,
    "documentRangeFormattingProvider" .= _documentRangeFormattingProvider sc,
    "documentOnTypeFormattingProvider" .= _documentOnTypeFormattingProvider sc,
    "renameProvider" .= _renameProvider sc,
    "documentLinkProvider" .= _documentLinkProvider sc,
    "executeCommandProvider" .= _executeCommandProvider sc
    ]

data InitializeResult = InitializeResult {
  _serverCapabilities :: ServerCapabilities
} deriving (Eq, Ord, Show)

-- | Default `ServerCapabilities` value indicating no capabilities whatsoever.
serverCapabilities :: ServerCapabilities
serverCapabilities = ServerCapabilities
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing
  Nothing

instance ToJSON InitializeResult where
  toJSON ir = object ["serverCapabilities" .= _serverCapabilities ir]

-- | Default `InitializeResult` value
initializeResult :: InitializeResult
initializeResult = InitializeResult serverCapabilities