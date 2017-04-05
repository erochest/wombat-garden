module Types where


import qualified Data.Text          as T

import           WombatGarden.Types


data Actions
  = Init   { initName       :: !T.Text
           , initParameters :: !Parameters
           , initPublish    :: !Bool
           }
  | Grow   { growName   :: !T.Text
           , growKey    :: !T.Text
           , growN      :: !(Maybe Int)
           , growApiUrl :: !String
           }
  | Report { reportName   :: !T.Text
           , reportOutput :: !FilePath
           }
  deriving (Show, Eq)
