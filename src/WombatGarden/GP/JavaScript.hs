{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module WombatGarden.GP.JavaScript where


import           Control.Lens
import           Control.Monad                  (join, replicateM)
import           Data.Char                      (chr, ord)
import           Data.Data
import           Data.Foldable
import           Generics.SOP
import qualified GHC.Generics                   as G
import           Language.JavaScript.Parser.AST
import           Numeric                        (showHex, showOct)

import           WombatGarden.Types


annot :: JSAnnot
annot  = JSNoAnnot

annot' :: GP e s JSAnnot
annot' = pure JSNoAnnot

newJSIdent :: GP e s JSIdent
newJSIdent = JSIdentName JSNoAnnot <$> newName

noJSIdent :: GP e s JSIdent
noJSIdent = pure JSIdentNone

maybeJSIdent :: GP e s JSIdent
maybeJSIdent = JSIdentName annot <$> maybeNewName

jsIdentifier :: GP e s JSExpression
jsIdentifier = JSIdentifier annot <$> name

newJSIdentifier :: GP e s JSExpression
newJSIdentifier = JSIdentifier annot <$> newName

maybeNewJSIdentifier :: GP e s JSExpression
maybeNewJSIdentifier = JSIdentifier annot <$> maybeNewName

data NewAssign = NewAssign { unNewAssign :: !JSExpression }
  deriving (Show, Eq, Data, Typeable, G.Generic)

instance Genetic NewAssign where
  randomGene = NewAssign <$> randomGene

newAssign :: GP e s JSExpression
newAssign = JSAssignExpression
            <$> newJSIdentifier
            <*> pure (JSAssign annot)
            <*> randomGene

newAssign' :: NewAssign -> GP e s JSExpression
newAssign' (NewAssign expression) = do
  new <- newJSIdentifier
  return $ JSAssignExpression new (JSAssign annot) expression

toCommaList :: [a] -> JSCommaList a
toCommaList xs = go $ reverse xs
  where
    go :: [x] -> JSCommaList x
    go []     = JSLNil
    go [y]    = JSLOne y
    go (y:ys) = JSLCons (go ys) annot y

newAssignList :: GP e s (JSCommaList JSExpression)
newAssignList = fmap toCommaList $ mapM newAssign' =<< randomGene

-- assign :: GP e s JSExpression
-- assign

deriving instance G.Generic JSAccessor

deriving instance G.Generic JSAnnot

deriving instance G.Generic JSArrayElement

deriving instance G.Generic JSAssignOp

deriving instance G.Generic JSAST

deriving instance G.Generic JSBinOp

deriving instance G.Generic JSBlock

deriving instance G.Generic x => G.Generic (JSCommaList x)

deriving instance G.Generic x => G.Generic (JSCommaTrailingList x)

deriving instance G.Generic JSExpression

deriving instance G.Generic JSIdent

deriving instance G.Generic JSObjectProperty

deriving instance G.Generic JSPropertyName

deriving instance G.Generic JSSemi

deriving instance G.Generic JSStatement

deriving instance G.Generic JSSwitchParts

deriving instance G.Generic JSTryCatch

deriving instance G.Generic JSTryFinally

deriving instance G.Generic JSUnaryOp

deriving instance G.Generic JSVarInitializer

instance Generic JSAccessor

instance Generic JSAnnot

instance Generic JSArrayElement

instance Generic JSAssignOp

instance Generic JSAST

instance Generic JSBinOp

instance Generic JSBlock

instance (G.Generic x, Generic x) => Generic (JSCommaList x)

instance (G.Generic x, Generic x) => Generic (JSCommaTrailingList x)

instance Generic JSExpression

instance Generic JSIdent

instance Generic JSObjectProperty

instance Generic JSPropertyName

instance Generic JSSemi

instance Generic JSStatement

instance Generic JSSwitchParts

instance Generic JSTryCatch

instance Generic JSTryFinally

instance Generic JSUnaryOp

instance Generic JSVarInitializer

instance Genetic JSAccessor where
  randomGene = gRandomGene

instance Genetic JSAnnot where
  randomGene = pure JSNoAnnot

instance Genetic JSArrayElement where
  randomGene = gRandomGene

instance Genetic JSAssignOp where
  randomGene = gRandomGene

instance Genetic JSAST where
  randomGene = gRandomGene

instance Genetic JSBinOp where
  randomGene = gRandomGene

instance Genetic JSBlock where
  randomGene = gRandomGene

instance (G.Generic x, Generic x, Genetic x)
         => Genetic (JSCommaList x) where
  randomGene = gRandomGene

instance (G.Generic x, Generic x, Genetic x)
         => Genetic (JSCommaTrailingList x) where
  randomGene = gRandomGene

instance Genetic JSExpression where
  randomGene =
    join $ choice [ JSIdentifier annot <$> identifier
                  , JSDecimal annot <$> decimal
                  , JSLiteral annot <$> literal
                  , JSHexInteger annot <$> hex
                  , JSOctal annot <$> octal
                  , JSStringLiteral annot <$> strLiteral
                  , JSRegEx annot <$> regex
                  , JSArrayLiteral annot <$> array <*> annot'
                  , JSAssignExpression
                    <$> fmap (JSIdentifier annot) maybeNewName
                    <*> randomGene
                    <*> randomGene
                  , JSCallExpression <$> randomGene <*> annot'
                    <*> randomGene <*> annot'
                  , JSCallExpressionDot <$> randomGene <*> annot'
                    <*> randomGene
                  , JSCallExpressionSquare <$> randomGene <*> annot'
                    <*> randomGene <*> annot'
                  , JSCommaExpression <$> randomGene <*> annot'
                    <*> randomGene
                  , JSExpressionBinary <$> randomGene <*> randomGene
                    <*> randomGene
                  , JSExpressionParen annot <$> randomGene <*> annot'
                  , JSExpressionPostfix <$> randomGene <*> randomGene
                  , JSExpressionTernary <$> randomGene <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                  , JSFunctionExpression annot
                    <$> newJSIdent
                    <*> annot' <*> randomGene <*> annot'
                    <*> randomGene
                  , JSMemberDot <$> randomGene <*> annot'
                    <*> randomGene
                  , JSMemberExpression <$> randomGene <*> annot'
                    <*> randomGene <*> annot'
                  , JSMemberNew annot <$> randomGene <*> annot'
                    <*> randomGene <*> annot'
                  , JSMemberSquare <$> randomGene <*> annot'
                    <*> randomGene <*> annot'
                  , JSNewExpression annot <$> randomGene
                  , JSObjectLiteral annot <$> randomGene <*> annot'
                  , JSUnaryExpression <$> randomGene <*> randomGene
                  , JSVarInitExpression <$> randomGene <*> randomGene
                  ]
    where
      identifier, decimal, literal, hex, octal, strLiteral
        , regex :: GP e s String

      identifier = name
      decimal = show <$> uniformR (-1024 :: Double, 1024 :: Double)
      literal = choice ["null", "true", "false", "this"]
      hex = (`showHex` "") <$> uniformR (-1024 :: Int, -1024 :: Int)
      octal = (`showOct` "") <$> uniformR (-1024 :: Int, -1024 :: Int)
      strLiteral =
        (`replicateM` (chr <$> uniformR (ord ' ', ord '~')))
        =<< uniformR (0, 1024)
      regex = strLiteral

      array :: GP e s [JSArrayElement]
      array = randomGene

instance Genetic JSIdent where
  randomGene = JSIdentName <$> randomGene <*> name

instance Genetic JSObjectProperty where
  randomGene = gRandomGene

instance Genetic JSPropertyName where
  randomGene = do
    names <- use gpsNames
    JSPropertyString <$> randomGene <*> choice (toList names)

instance Genetic JSSemi where
  randomGene = gRandomGene

instance Genetic JSStatement where
  randomGene =
    join $ choice [ JSStatementBlock annot <$> randomGene <*> annot'
                    <*> randomGene
                  , JSBreak annot <$> noJSIdent <*> randomGene
                  , JSConstant annot <$> newAssignList <*> randomGene
                  , JSContinue annot <$> noJSIdent <*> randomGene
                  , JSDoWhile annot <$> randomGene <*> annot'
                    <*> annot' <*> randomGene <*> annot' <*> randomGene
                  , JSFor annot annot <$> newAssignList <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                    <*> annot' <*> randomGene
                  , JSForIn annot annot <$> newJSIdentifier
                    <*> randomGene <*> randomGene <*> annot'
                    <*> randomGene
                  , JSForVar annot annot annot <$> newAssignList
                    <*> annot' <*> randomGene <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                  , JSForVarIn annot annot annot <$> newJSIdentifier
                    <*> randomGene <*> randomGene <*> annot'
                    <*> randomGene
                  , JSFunction annot <$> newJSIdent <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                    <*> randomGene
                  , JSIf annot annot <$> randomGene <*> annot'
                    <*> randomGene
                  , JSIfElse annot annot <$> randomGene <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                  , JSLabelled <$> newJSIdent <*> annot'
                    <*> randomGene
                  , pure (JSEmptyStatement annot)
                  , JSExpressionStatement <$> randomGene
                    <*> randomGene
                  , JSAssignStatement <$> jsIdentifier
                    <*> randomGene <*> randomGene <*> randomGene
                  , JSMethodCall <$> randomGene <*> annot'
                    <*> randomGene <*> annot' <*> randomGene
                  , JSReturn annot <$> randomGene <*> randomGene
                  , JSSwitch annot annot <$> randomGene <*> annot'
                    <*> annot' <*> randomGene <*> annot'
                    <*> randomGene
                  , JSThrow annot <$> randomGene <*> randomGene
                  , JSTry annot <$> randomGene <*> randomGene
                    <*> randomGene
                  , JSVariable annot <$> newAssignList <*> randomGene
                  , JSWhile annot annot <$> randomGene <*> annot'
                    <*> randomGene
                  , JSWith annot annot <$> randomGene <*> annot'
                    <*> randomGene <*> randomGene
                  ]

instance Genetic JSSwitchParts where
  randomGene = gRandomGene

instance Genetic JSTryCatch where
  randomGene = gRandomGene

instance Genetic JSTryFinally where
  randomGene = gRandomGene

instance Genetic JSUnaryOp where
  randomGene = gRandomGene

instance Genetic JSVarInitializer where
  randomGene = gRandomGene
