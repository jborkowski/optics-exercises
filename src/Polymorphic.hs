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
