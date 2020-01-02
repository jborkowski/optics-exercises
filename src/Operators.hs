{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Operators where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Payload =
  Payload { _weight :: Int
          , _cargo :: String
          } deriving (Show)

data Ship =
  Ship { _payload :: Payload
       } deriving (Show)

makeLenses ''Payload
makeLenses ''Ship

serenity :: Ship
serenity = Ship (Payload 50000 "Livestock")

-- Operatoras:
-- flip view - ^. - s -> Lens' s a -> a
oldView = view (payload . cargo) serenity

-- Remember about flipped order
operatorView = serenity ^. payload . cargo

-- set       - .~ - Lens s t a b -> b -> s -> t
oldSet = set (payload . cargo) "Medicine" serenity
operatorSet = (payload . cargo .~ "Medicine") $ serenity

-- sometimes $ approach gets a bit clumsy when we want to compose multiple lens
-- There is another operator called (&) - flipper funcion applciation -- in other languages it's called 'pipe'
-- I read & as ‘and-then’, and read .~ as set, so if we add a bit of filler we end up with:

-- “Take serenity and then, regarding its payload’s cargo, set it to Medicine”
operatorFlippedSet = serenity & payload . cargo .~ "Medicine"

-- multi operations
multi = serenity 
  & payload . cargo .~ "Chocolate" 
  & payload . weight .~ 2310


-- over      - %~ - Lens s t a b -> (a -> b) -> s -> t
operatorOver = serenity
  & payload . weight %~ subtract 20
  & payload . cargo .~ "Chocolate"

-- MORE operators I can find on hoogle
-- e.g.
-- +~ add
-- -~ Subtract
-- *- multiply
-- //~ divide
-- ^~ Rise the numberic
-- ^^~ factorial
-- **~ Floating
-- ||~ &&~ logical and and or
-- <>~ mampend a value  onto the focus (right)

-- MODIFIERS
data Thermometer =
  Thermomenter { _temperature :: Int
               } deriving Show
makeLenses ''Thermometer

operatorModify = Thermomenter 20 & temperature <+~ 13



--Exercises:
data Gate =
    Gate { _open    :: Bool
         , _oilTemp :: Float
         } deriving Show
makeLenses ''Gate

data Army =
    Army { _archers :: Int
         , _knights :: Int
         } deriving Show
makeLenses ''Army

data Kingdom =
    Kingdom { _name :: String
            , _army :: Army
            , _gate :: Gate
            } deriving Show
makeLenses ''Kingdom


duloc :: Kingdom
duloc =
    Kingdom { _name = "Duloc"
            , _army = Army { _archers = 22
                           , _knights = 14
                           }
            , _gate = Gate { _open    = True
                           , _oilTemp = 10.0
                           }
            }


goalA = duloc
  & name <>~ ": a perfect place"
  & army . knights +~ 28
  & gate . open &&~ False

goalB = duloc
  & name <>~ "instein"
  & army . archers -~  5
  & army . knights +~ 12
  & gate . oilTemp *~ 10

goalC = duloc
  & gate . oilTemp //~ 2
  & name <<>~ ": home" -- mappend a monoidal value onto the end of the target of a Lens and return the result.
  & _2 . name  <>~ " of the talking Donkeys" -- Modify the target of a monoidally valued by mappending another value.

ex21 = (False, "opossums") & _1 ||~ True

ex22 = 2 & id *~ 3

ex23 = ((True, "Dudley"), 55.0)
  & _1 . _2 <>~ " - the worst"
  & _2 -~ 15
  & _2 //~ 2
  & _1 . _2 %~ map toUpper
  & _1 . _1 &&~ False
