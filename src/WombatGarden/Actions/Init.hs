{-# LANGUAGE OverloadedStrings #-}


module WombatGarden.Actions.Init where


import           Control.Error
import           Control.Exception.Safe
import           Control.Monad
import           Data.Aeson               (encode)
import           Data.Bifunctor
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Text                as T
import qualified Data.Text.Lazy.IO        as TLIO
import           System.Directory
import           System.IO
import           System.Process

import           WombatGarden.GP.Generate
import           WombatGarden.Types


initAction :: T.Text -> Parameters -> Bool -> Script ()
initAction repoName params publish = do
  scriptIO $ createDirectoryIfMissing True name'
  scriptIO $ withCurrentDirectory name' $ runScript $ do
    git_ "init" []
    when publish $
      hub_ "create" []
    scriptIO $ BL.writeFile "params.json" $ encode params
    scriptIO $ BL.writeFile "data-log.json" mempty

    hoistEither (first (T.pack . displayException)
                 $ runGP params initializePopulation)
      >>= scriptIO . mapM_ (uncurry TLIO.writeFile)

    gitAdd "."
    gitCommit "garden set up"
    git_ "push" ["-u", "origin", "master"]
  where
    name' = T.unpack repoName

type ProcessOut = Script ( Maybe Handle, Maybe Handle, Maybe Handle
                         , ProcessHandle
                         )

git :: String -> [String] -> ProcessOut
git = command "git"

git_ :: String -> [String] -> Script ()
git_ cmd = void . git cmd

gitAdd :: FilePath -> Script ()
gitAdd = git_ "add" . pure

gitCommit :: String -> Script ()
gitCommit msg = git_ "commit" ["-m", msg]

hub :: String -> [String] -> ProcessOut
hub = command "hub"

hub_ :: String -> [String] -> Script ()
hub_ cmd = void . hub cmd

command :: String -> String -> [String] -> ProcessOut
command cmd0 cmd1 args =
  scriptIO $ createProcess $ proc cmd0 $ cmd1 : args
