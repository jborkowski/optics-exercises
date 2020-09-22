{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Isos where

import Control.Lens
import Numeric.Lens
import Data.List (transpose)
import Data.Char (isUpper, toUpper, toLower)

-- Exercises
-- Fill in the blank

ex11 = ("Beauty", "Age") ^. swapped
-- result: ("Age", "Beauty")

ex12 = 50 ^. from (adding 10)
-- result: 40
ex13 = 0 & multiplying 4 +~ 12
-- result: 3.0
ex14 = 0 & adding 10 . multiplying 2 .~ 24
-- result: 2.0
ex15 = [1,2,3] & reversed %~ drop 1
-- result: [1,2]
ex16 = (view flipped (++)) [1,2] [3,4]
--result: [3,4,1,2]
ex17 = [1,2,3] ^. reversed
--result: [3,2,1]

ex18 = [[1,2,3], [10,20,30]] & involuted transpose %~ drop 1
--result [[2,3],[20,30]]

switchCase c = if isUpper c then toLower c else toUpper c
ex19 = (32, "Hi") & _2 . involuted (fmap switchCase) .~ ("hELLO" :: String)
-- result: (32,"Hello")

fahrenheit :: Iso' Double Double
fahrenheit = iso celsiusToF fahrenheitToC
  where
    fahrenheitToC :: Double -> Double
    fahrenheitToC f = (f - 32) * (5/9)
    celsiusToF :: Double -> Double
    celsiusToF c = (c * (9/5)) + 32

---------------------------------------------------------------------------------
--
--  Projected Isos - Exercises
--
---------------------------------------------------------------------------------

ex21 = ("Beauty", "Age") ^. mapping reversed . swapped
-- result: ("egA","Beauty")

ex22 = [True, False, True] ^. mapping (involuted  not)
-- result:  [False,True,False]

ex23 = [True, False, True] & mapping (involuted not) %~ filter id
-- result: [False]

isNot :: Int -> Int
isNot = not ^. dimapping enum (from enum)

isNot' :: Int -> Int
isNot' = enum %~ not
