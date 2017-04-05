{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}


module WombatGarden.Types where


import           Control.Lens             hiding (to)
import           Control.Monad
import           Control.Monad.RWS.Strict hiding (All)
import           Control.Monad.ST
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Data
import qualified Data.HashSet             as S
import qualified Data.List                as L
import           Data.Vector              as V hiding (replicateM, toList)
import           Generics.SOP
import qualified GHC.Generics             as GHC
import qualified System.Random.MWC        as R
import Data.Foldable
import Control.Error
import Control.Exception.Safe


data Parameters
  = Parameters
  { _paramPopulation :: !Int
  , _paramNewName    :: !Double
  , _paramCarryOver  :: !Int
  , _paramNewRandom  :: !Int
  , _paramCrossOver  :: !Double
  , _paramMutation   :: !Double
  , _paramDepth      :: !Int
  } deriving (Show, Eq, GHC.Generic, Data, Typeable)

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

data GPState s =
  GPS
  { _gpsGen   :: !(R.GenST s)
  , _gpsNames :: !(S.HashSet String)
  }
makeLenses ''GPState

newtype GP e s a =
  GP { unGP :: ExceptT e (RWST Parameters () (GPState s) (ST s)) a }
  deriving ( Functor, Applicative, Monad, MonadState (GPState s)
           , MonadReader Parameters
           )

runGP :: Exception e => Parameters -> (forall s. GP e s a) -> Either e a
runGP params gp = fst $ runST $
  evalRWST (runExceptT (unGP gp)) params . (`GPS` mempty) =<< R.create

-- | This runs the action and returns the result, but it throws away
-- any changes to state that it makes.
forkState :: MonadState s m => m a -> m a
forkState action = do
  s <- get
  a <- action
  put s
  return a

getNames :: GP e s (S.HashSet String)
getNames = use gpsNames

addName :: String -> GP e s ()
addName n = modifying gpsNames (S.insert n)

-- | Creates a new name, saves it to the set of names, and returns it.
newName :: GP e s String
newName = state $ \GPS{_gpsGen=gen,_gpsNames=names} ->
  let new    = "name" <> show (S.size names)
      names' = S.insert new names
  in  (new, GPS gen names')

name :: GP e s String
name = choice . toList =<< getNames

maybeNewName :: GP e s String
maybeNewName = do
  x :: Double <- uniform
  rate <- view paramNewName
  if x <= rate
    then newName
    else name

uniform :: R.Variate a => GP e s a
uniform = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniform (_gpsGen g)

uniformR :: R.Variate a => (a, a) -> GP e s a
uniformR range = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniformR range (_gpsGen g)

uniformVector :: R.Variate a => Int -> GP e s (V.Vector a)
uniformVector size = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniformVector (_gpsGen g) size

choice :: [a] -> GP e s a
choice xs = (xs !!) <$> uniformR (0, L.length xs - 1)

class Genetic ast where
  randomGene :: GP e s ast

instance Genetic x => Genetic [x] where
  randomGene = (`replicateM` randomGene) =<< uniformR (1, 1024)

instance Genetic x => Genetic (Maybe x) where
  randomGene = do
    n <- uniform :: (GP e s Double)
    if n < 0.5
      then pure Nothing
      else Just <$> randomGene

gRandomGene :: forall e s a. (All2 Genetic (Code a), Generic a) => GP e s a
gRandomGene =
  liftM to $ hsequence =<< choice (apInjs_POP $ hcpure genetic randomGene)
  where
    genetic = Proxy :: Proxy Genetic
