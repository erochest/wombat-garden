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
import qualified Control.Exception.Safe as E
import Control.Monad.Catch
import Control.Exception.Base
import Data.Maybe

import Debug.Trace


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
  { _gpsNames :: !(S.HashSet String)
  , _gpsDepth :: !Int
  , _gpsGen   :: !(R.GenST s)
  }
makeLenses ''GPState

newtype GP s a =
  GP { unGP :: ExceptT SomeException (RWST Parameters () (GPState s) (ST s)) a }
  deriving ( Functor, Applicative, Monad, MonadState (GPState s)
           , MonadReader Parameters
           )

instance MonadThrow (GP s) where
  throwM e = GP $ hoistEither $ Left $ toException e

instance MonadCatch (GP s) where
  catch action handler = do
    v <- GP $ ExceptT $ fmap Right $ RWST $
      runRWST (runExceptT $ unGP action)
    case v of
      Right v' -> return v'
      Left  e  -> maybe
                  (E.throwM
                   (toException
                    (AssertionFailed "Invalid Exception type")))
                  handler
                  $ fromException e

instance MonadMask (GP s) where
  -- | Async shouldn't be an option in the ST monad, so I can just do
  -- the simplest thing possible.
  mask a = a id
  uninterruptibleMask a = a id

runGP :: Parameters -> (forall s. GP s a) -> Either SomeException a
runGP params gp = fst $ runST $
  evalRWST (runExceptT (unGP gp)) params . GPS mempty 0 =<< R.create

-- | This runs the action and returns the result, but it throws away
-- any changes to state that it makes.
forkState :: MonadState s m => m a -> m a
forkState action = do
  s <- get
  a <- action
  put s
  return a

-- | This checks the depth in the current state against the maximum in
-- the parameters. If it can go deeper, it will increment the depth
-- and run the first action; if it cannot, it will run the second
-- action, which will probably be a subset of the first.
withDepth :: GP s a -> GP s a -> GP s a
withDepth _moar _less = do
  _maxDepth <- view paramDepth
  -- bracket
  --   (do
  --       d <- use gpsDepth
  --       assign gpsDepth (d + 1)
  --       return d)
  --   (assign gpsDepth)
  --   (\d -> if d < maxDepth then moar else less)
  undefined

getNames :: GP s (S.HashSet String)
getNames = use gpsNames

addName :: String -> GP s ()
addName n = modifying gpsNames (S.insert n)

-- | Creates a new name, saves it to the set of names, and returns it.
newName :: GP s String
newName = state $ \gps@GPS{_gpsNames=names} ->
  let new    = "name" <> show (S.size names)
      names' = S.insert new names
  in  (new, gps & gpsNames .~ names')

name :: GP s String
name = choice . toList =<< getNames

maybeNewName :: GP s String
maybeNewName = do
  x :: Double <- uniform
  rate <- view paramNewName
  if x <= rate
    then newName
    else name

uniform :: R.Variate a => GP s a
uniform = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniform (_gpsGen g)

uniformR :: R.Variate a => (a, a) -> GP s a
uniformR range = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniformR range (_gpsGen g)

uniformVector :: R.Variate a => Int -> GP s (V.Vector a)
uniformVector size = GP . ExceptT . RWST $ \_ g ->
  (, g, ()) . Right <$> R.uniformVector (_gpsGen g) size

choice :: [a] -> GP s a
choice xs = (xs !!) <$> uniformR (0, L.length xs - 1)

class Genetic ast where
  randomGene :: GP s ast

instance Genetic x => Genetic [x] where
  randomGene = (`replicateM` randomGene) =<< uniformR (1, 1024)

instance Genetic x => Genetic (Maybe x) where
  randomGene = do
    n <- uniform :: (GP s Double)
    if n < 0.5
      then pure Nothing
      else Just <$> randomGene

gRandomGene :: forall s a. (All2 Genetic (Code a), Generic a, Typeable a)
            => GP s a
gRandomGene = do
  traceM . ("gRandomGene: " L.++) . show $ typeRep a'
  liftM to $ hsequence =<< choice (apInjs_POP $ hcpure genetic randomGene)
  where
    a' = Proxy :: Proxy a
    genetic = Proxy :: Proxy Genetic
