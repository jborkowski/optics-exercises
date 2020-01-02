{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Polymorphic where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Promotion a =
  Promotion { _item :: a
            , _discountPercentage :: Double
            }
  deriving Show

item :: Lens (Promotion a) (Promotion b) a b
item = lens getter setter
  where
    getter :: Promotion a -> a
    getter = _item
    setter :: Promotion a -> b -> Promotion b
    setter promo newItem = promo{_item = newItem}

data Preferences a =
  Preferences { _best :: a
              , _worst :: a
              }
  deriving Show



-- vorpal :: Lens (Vorpal x) (Vorpal y) (x,y)

--favourites :: Lens (Preferences a) (Preferences b) (a,a) (b,b)
--favourites = lens getter setter 
--  where
--    getter :: Preferences a -> (a,a)
--    getter = (_best, _worst)
--    setter :: Preferences a -> b -> Preferences b
--    setter pref new = undefined

data Result e =
  Result { _lineNumber :: Int
         , _result :: Either e String
         }
  deriving Show

data Predicate a =
  Predicate (a -> Bool)

result :: Lens (Result a) (Result b) (Either a String) (Either b String)
result = lens getter setter
  where
    getter :: (Result a) -> (Either a String)
    getter = _result
    setter :: (Result a) -> (Either b String) -> (Result b)
    setter predicat newResult = predicat{ _result = newResult }


-- It’s thinking time! Is it possible to change more than one type variable at a time using a polymorphic
data UnsafeRun e a =
    Error e
  | Success a
  deriving Show

run :: Lens (UnsafeRun e a) (UnsafeRun f b) (Either e a) (Either f b)
run = lens getter setter
  where
    getter (Error e) = Left e
    getter (Success a) = Right a
    setter _ (Left f) = Error f
    setter _ (Right b) = Success b

predicate :: Lens (Predicate a) (Predicate b) (a -> Bool) (b -> Bool)
predicate = lens getter setter
  where
    getter :: (Predicate a) -> (a -> Bool)
    getter = undefined -- I have no idea how can I satisfy this signature :) 
    setter :: (Predicate a) -> (b -> Bool) -> (Predicate b)
    setter pred new = Predicate new
