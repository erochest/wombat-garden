{-# LANGUAGE OverloadedStrings #-}


module WombatGarden.Actions.Init where


import           Control.Error
import qualified Data.Text     as T


initAction :: T.Text -> Int -> Int -> Int -> Double -> Double -> Int
           -> Script ()
initAction _name _population _carryOver _newRandom _crossOver _mutation _depth = undefined
