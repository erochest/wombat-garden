{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module WombatGarden.GP.Generate where


import           Control.Lens
import           Control.Monad
import           Data.Bifunctor
import           Data.Text.Format
import qualified Data.Text.Lazy                 as TL
import           Language.JavaScript.Parser
import           Language.JavaScript.Parser.AST

import           WombatGarden.GP.JavaScript     ()
import           WombatGarden.Types


initializePopulation :: GP s [(String, TL.Text)]
initializePopulation = do
  n <- view paramPopulation
  setNames
  population <- map ( wrapAll
                      . renderToText
                      . (`JSAstStatement` JSNoAnnot)
                      . wrapMain)
    <$> replicateM n (forkState randomGene)
  let getFilename x =
        TL.unpack $ format "gene-{}-{}.js" ( left 6 '0' (1 :: Int)
                                           , left 3 '0' (x :: Int))

  return $ fmap (first getFilename) $ zip [1..] population

setNames :: GP s ()
setNames = assign gpsNames [ "main", "state", "timeLeftFn"
                           , "getGlobalCoords", "getLocalCoords"
                           , "getGlobalDimensions", "getSavedState"
                           , "getArena", "getContentsAt"
                           , "isOpen", "returnType", "isWombat"
                           , "isZakano", "isWood", "isSteel", "isFlame"
                           , "isSmoke", "isFog", "isFood", "isPoison"
                           , "Math", "E", "LN2", "LN10", "LOG2E"
                           , "LOG10E", "PI", "SQRT1_2", "SQRT2"
                           , "abs", "acos", "acosh", "asin", "asinh"
                           , "atan", "atanh", "atan2", "cbrt", "ceil"
                           , "clz32", "cos", "cosh", "exp", "expml"
                           , "floor", "fround", "hypot", "imul"
                           , "log", "log1p", "log10", "log2", "max"
                           , "min", "pow", "random", "round", "sign"
                           , "sin", "sinh", "tan", "tanh", "trunc"
                           ]

wrapMain :: JSBlock -> JSStatement
wrapMain body =
  JSFunction JSNoAnnot (JSIdentName JSNoAnnot "main") JSNoAnnot JSLNil
             JSNoAnnot body (JSSemi JSNoAnnot)

wrapAll :: TL.Text -> TL.Text
wrapAll body =
  mconcat [ "(function(state, timeLeftFn) {\n"
          , getGlobalCoords
          , getLocalCoords
          , getGlobalDimensions
          , getSavedState
          , getArena
          , getContentsAt
          , isOpen
          , returnType
          , isWombat
          , isZakano
          , isWood
          , isSteel
          , isFlame
          , isSmoke
          , isFog
          , isFood
          , isPoison
          , body
          , "});"
          ]
  where
    getGlobalCoords = "function getGlobalCoords() {\
                      \ return state['global-coords']; }\n"
    getLocalCoords = "function getLocalCoords() {\
                     \ return state['local-coords']; }\n"
    getGlobalDimensions = "function getGlobalDimensions() {\
                          \ return state['global-dimensions']; }\n"
    getSavedState = "function getSavedState() {\
                    \ return state['saved-state']; }\n"
    getArena = "function getArena() {\
               \ return state['arena']; }\n"
    getContentsAt = "function getContentsAt(x, y) {\
                    \ return state['arena'][y][x]; }\n"
    isOpen = "function isOpen(x, y) {\
             \ return state['arena'][y][x]['type'] == 'open'; }\n"
    returnType = "function returnType(x, y, name) {\
                 \ var block = state['arena'][y][x];\
                 \ if (block['type'] == name) {\
                 \   return block;\
                 \ } else {\
                 \   return false;\
                 \ }\
                 \ }\n"
    isWombat = "function isWombat(x, y) { \
               \ return returnType(x, y, 'wombat'); }\n"
    isZakano = "function isZakano(x, y) {\
               \ return returnType(x, y, 'zakano'); }\n"
    isWood = "function isWood(x, y) { \
             \ return returnType(x, y, 'wood-barrier'); }\n"
    isSteel = "function isSteel(x, y) {\
              \ return returnType(x, y, 'steel-barrier'); }\n"
    isFlame = "function isFlame(x, y) {\
              \ return returnType(x, y, 'flame'); }\n"
    isSmoke = "function isSmoke(x, y) {\
              \ return returnType(x, y, 'smoke'); }\n"
    isFog = "function isFog(x, y) { \
            \ return state['arena'][y][x]['type'] == 'fog'; }\n"
    isFood = "function isFood(x, y) {\
             \ return returnType(x, y, 'food'); }\n"
    isPoison = "function isPoison(x, y) {\
               \ return returnType(x, y, 'poison'); }\n"

