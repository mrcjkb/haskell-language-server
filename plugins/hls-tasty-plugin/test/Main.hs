{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           Control.Lens               (_Just, folded, preview, view, (^.),
                                             (^..), (^?))
import           Control.Monad              (join)
import           Data.Aeson                 (Value (Object), fromJSON, object,
                                             (.=))
import           Data.Aeson.Types           (Pair, Result (Success))
import           Data.List                  (isInfixOf)
import           Data.List.Extra            (nubOrdOn)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import qualified Data.Text                  as T
import           Ide.Plugin.Config          (Config)
import qualified Ide.Plugin.Config          as Plugin
import qualified Ide.Plugin.Tasty            as Tasty
import           Language.LSP.Protocol.Lens (command, range, title)
import           System.FilePath            ((<.>), (</>))
import           Test.Hls
import qualified Test.Hls.FileSystem        as FS

main :: IO ()
main = defaultTestRunner tests

tastyPlugin :: PluginTestDescriptor Tasty.Log
tastyPlugin = mkPluginTestDescriptor Tasty.descriptor "tasty"

tests :: TestTree
tests = testGroup "tasty"
  [ testCase "Produces Run test code lenses" $
      runSessionWithServerInTmpDir def tastyPlugin (mkFs $ FS.simpleCabalProject [ "T1.hs", "test.cabal" ]) $ do
        doc <- openDoc "T1.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ map (preview $ command . _Just . title) lenses @?= [Just "Run tests..."]
  ]

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-tasty-plugin" </> "test" </> "testdata"

mkFs :: [FS.FileTree] -> FS.VirtualFileTree
mkFs = FS.mkVirtualFileTree testDataDir
