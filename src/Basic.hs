{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Basic where

import Control.Lens
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Leading undersocres in field names, That's no accident.
-- Lenses provide more convenient and powerful interface than stock Haskell  
data Ship =
  Ship { _name :: String
       , _numCrew :: Int
       }
  deriving (Show)

-- auto gen lenses
makeLenses ''Ship

-- Lens provide lens-builder
-- lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
-- where:
-- (s -> a) - getter function
-- (s -> b -> t) - setter function

getNumCrew :: Ship -> Int
getNumCrew = _numCrew

setNumCrew :: Ship -> Int -> Ship
setNumCrew ship newCrewNum =
  ship{ _numCrew = newCrewNum }

-- Not Used, auto genereted
-- numCrew :: Lens' Ship Int
-- numCrew = lens getNumCrew setNumCrew

-- Answers:
-- *structure* is represent by <s> letter
-- *focus* is represent by <a> letter
-- Getter and setter are required to create lens

getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName ship newName =
  ship{ _name = newName }
-- Not used, auto generated
-- name :: Lens' Ship String
-- name = lens getName setName

purplePearl :: Ship
purplePearl =
  Ship { _name = "Purple Pearl"
       , _numCrew = 35
       }



type Wand = String
type Book = String
type Potion = String

data Inventory =
  Inventory { _wand :: Wand
            , _book :: Book
            , _potions :: [Potion]}

makeLenses ''Inventory

-- gazork :: Functor f => (Spuzz -> f Spuzz) -> Chumble -> f Chumble
-- Fill in the blanks:
-- gazork :: Lens' Chumble Spuzz

data Pet =
  Pet { _petName :: String
      , _petType :: String
      }
  deriving Show

makeLenses ''Pet

getPetName :: Pet -> String
getPetName pet = view petName pet


getConditional :: (Bool, a, a) -> a
getConditional tuple = case tuple of (True, a, _)  -> a 
                                     (False, _, a) -> a

setConditional :: (Bool, a, a) -> a -> (Bool, a, a)
setConditional tuple newValue = case tuple of (True, _, a)  -> (True, newValue, a)
                                              (False, a, _) -> (False, a, newValue)  

conditional :: Lens' (Bool, a, a) a
conditional = lens getConditional setConditional

data Err =
    ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }

  
