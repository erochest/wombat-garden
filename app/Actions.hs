{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Actions where


import           Control.Error

import           WombatGarden.Actions.Grow
import           WombatGarden.Actions.Init
import           WombatGarden.Actions.Report

import           Types


action :: Actions -> Script ()

action Init{..}   = initAction initName initParameters initPublish
action Grow{..}   = growAction growName growKey growN growApiUrl
action Report{..} = reportAction reportName reportOutput
