{-# LANGUAGE OverloadedStrings #-}


module WombatGarden.Actions.Init where


import           Control.Error
import           Control.Monad
import           Data.Aeson           (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import           System.Directory
import           System.FilePath
import           System.IO
import           System.Process

import           WombatGarden.Types


initAction :: T.Text -> Int -> Int -> Int -> Double -> Double -> Int
           -> Script ()
initAction name population carryOver newRandom crossOver mutation
           depth = do
  scriptIO $ createDirectoryIfMissing True name'
  scriptIO $ withCurrentDirectory name' $ runScript $ do
    git_ "init" []
    hub_ "create" []
    scriptIO $ BL.writeFile (name' </> "params.json") $ encode params
    scriptIO $ BL.writeFile (name' </> "data-log.json") mempty

    -- TODO: generate first generation
    void $ undefined

    gitAdd "."
    gitCommit "garden set up"
    git_ "push" ["-u", "origin", "master"]
  where
    name' = T.unpack name
    params = Parameters population carryOver newRandom crossOver
                        mutation depth

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
