module Types where


import qualified Data.Text as T


data Actions
  = Init   { initName       :: !T.Text
           , initPopulation :: !Int
           , initCarryOver  :: !Int
           , initNewRandom  :: !Int
           , initCrossOver  :: !Double
           , initMutation   :: !Double
           , initDepth      :: !Int
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
