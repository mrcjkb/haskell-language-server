{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid restricted function" #-}

module Ide.Plugin.Tasty (descriptor, Log) where

import Data.Map.Strict as Map
import Data.Aeson
import Data.Text qualified as T
import Development.IDE
import Development.IDE.Core.PluginUtils
import GHC.Generics
import Ide.Plugin.Error
import Ide.Types
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Control.Monad.IO.Class (liftIO)
import Debug.Trace (traceShow)
import GHC.Prelude (pprTraceM)
import Development.IDE.GHC.Compat.Outputable (ppr)
import Development.IDE.GHC.Compat (HieASTs(..))

data Log
    deriving stock Show

instance Pretty Log where
    pretty _ = pretty ("todo" :: T.Text)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "A plugin for finding tasty test targets")
        { pluginCommands = [runTestCommand]
        , pluginHandlers =
            mconcat
                [ mkPluginHandler SMethod_TextDocumentCodeLens codeLensProvider
                ]
        }

data RunTestParams
    = RunTestParams
    { description :: T.Text
    , originatingFile :: Uri
    }
    deriving stock (Generic)
    deriving anyclass (FromJSON)

runTestCommand :: PluginCommand IdeState
runTestCommand =
    PluginCommand
        { commandId = runTestCommandId
        , commandDesc = "Run test command"
        , commandFunc = execRunTestCommand
        }

runTestCommandId :: CommandId
runTestCommandId = "RunTest"

-- TODO: Send WorkspaceExecuteCommand request
execRunTestCommand :: CommandFunction IdeState RunTestParams
execRunTestCommand = undefined

codeLensProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeLens
codeLensProvider
    state
    pId
    CodeLensParams{_textDocument = TextDocumentIdentifier{_uri}} = do
        nfp <- getNormalizedFilePathE _uri
        (HAR _ hieAsts refMap typeRefs _) <- runActionE "tasty.runTestsLens" state $ useE GetHieAst nfp
        -- hsc <- runActionE "tasty.runTestsLens" state $ useE GhcSessionDeps nfp


        -- pprTraceM "HAR" $ ppr refMap
        pprTraceM "AST" $ ppr $ Map.toList $ getAsts hieAsts
        -- traceShow ("AST", hieAst) $
        pure $ InL mempty
        -- pure $ InL mempty
