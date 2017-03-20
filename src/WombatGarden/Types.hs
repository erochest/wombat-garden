{-# LANGUAGE DeriveDataTypeable #-}
-- {-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric      #-}
-- {-# LANGUAGE DeriveTraversable          #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE OverloadedLists            #-}
-- {-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE RankNTypes                 #-}
-- {-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell    #-}


module WombatGarden.Types where


import           Control.Lens
import           Data.Data
-- import qualified Data.Text              as T
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.List        as L
import           GHC.Generics     hiding (to)


data Parameters
  = Parameters
  { _paramPopulation :: !Int
  , _paramCarryOver  :: !Int
  , _paramNewRandom  :: !Int
  , _paramCrossOver  :: !Double
  , _paramMutation   :: !Double
  , _paramDepth      :: !Int
  } deriving (Show, Eq, Generic, Data, Typeable)

makeClassy ''Parameters

paramOptions :: Options
paramOptions = defaultOptions
               { fieldLabelModifier = camelTo2 '_' . L.drop 6
               }

instance ToJSON Parameters where
  toJSON     = genericToJSON     paramOptions
  toEncoding = genericToEncoding paramOptions

instance FromJSON Parameters where
  parseJSON = genericParseJSON paramOptions
