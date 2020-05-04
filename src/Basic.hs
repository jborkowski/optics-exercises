{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Basic where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Unsound
import           Data.Char
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T

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
  deriving (Eq)

type UserId = String
type UserName = String

data Session =
  Session { _userId :: UserId
          , _userName :: UserName
          , _createdTime :: String
          , _expiryTime :: String
          }
  deriving (Show, Eq)

makeLenses ''Session

userInfo :: Lens' Session (UserId, UserName)
userInfo = lensProduct userId userName


exampleSession :: Session
exampleSession =
  Session { _userId = "USER1233"
          , _userName = "Joey Tribbiani"
          , _createdTime = "2019-12-1"
          , _expiryTime = "2020-12-1"
          }

alongsideUserId :: Lens' Session (Session, UserId)
alongsideUserId = lensProduct id userId

executionAppender :: Lens' ([a], a) a
executionAppender = lens getCnt setCnt
  where
    getCnt (_, a) = a
    setCnt (ls, a) newVal = ((a:ls), newVal)

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = (ReallyBadError newMessage)
    setMsg (ExitCode code) _ = ExitCode code


badError = ReallyBadError { _msg = "FatalError" }
exitCode = ExitCode { _code = 3 }
newMessage = "New Fatal Error"
newMessage2 = "NullPointerException"

setGet1 =
  view msg (set msg newMessage badError) == newMessage

setGet2 =
  view msg (set msg newMessage exitCode) == newMessage

setSet1 =
  set msg newMessage2 (set msg newMessage badError) == set msg newMessage2 badError

setSet2 =
  set msg newMessage2 (set msg newMessage exitCode) == set msg newMessage2 exitCode


-- From Answers

flipper :: Lens' (Bool, a, a) a
flipper = lens getter setter
  where
    getter (True, a, _)  = a
    getter (False, _, a) = a
    setter (True,  a, b) newVal = (False, newVal, b)
    setter (False, a, b) newVal = (True, a, newVal)
