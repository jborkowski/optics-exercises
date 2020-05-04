{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
module Prism where

import           Control.Lens
import qualified Data.Set     as S

data ContactInfo =
      Email String
    | Telephone Int
    | Address String String String

makePrisms ''ContactInfo

-- Will generate following prisms:
-- _Email Prism' ContactInfo String
-- _Telephone Prism' ContactInfo Int
-- _Address Prism' ContactInfo (String, String, String)

ex11 = Right 35 & _Right +~ 5
-- Res ex11: Right 40

ex12 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] ^.. folded . _Just
-- Res ex12: ["Mind","Power","Soul","Time"]

ex13 = [Just "Mind", Just "Power", Nothing, Just "Soul", Nothing, Just "Time"] & traversed . _Just <>~ " Stone"
-- Res ex13: [ Just "Mind Stone"
--          , Just "Power Stone”“, Nothing
--          , Just "Soul Stone"
--          , Nothing
--          , Just "Time Stone"
--          ]
ex14 = Left (Right True, "Eureka!") & _Left . _1 . _Right %~ not
-- Res ex14: Left (Right False, "Eureka!")

ex15 = _Cons # ("Do",["Re", "Mi"])
-- Res ex15: ["Do", "Re", "Mi"]

ex16 = isn't (_Show :: Prism' String Int) "not an int"
-- Res ex16: True

-- Write an expression to get the output from the provided input.

input1 = (Just 1, Nothing, Just 3)
output1 = input1 ^.. each . _Just
-- expected: [1, 3]

input2 = ('x', "yz")
output2 = _Cons # input2
-- expected: "xzy"

input3 = "do the hokey pokey"
output3 = _Left . _Just . _Right # input3
-- expected: Left (Just (Right "do the hokey pokey"))


-- Custom Prisms - Exercises
-- 1. Try to write a custom prism for matching on the tail of a list:

-- _Tail :: Prism' [a] [a]
-- Is this possible? Why or why not?

-- Answer:
-- It isn't possible, I don't have enough information to run this prim in reverse.


-- 2. Implement _Cons for lists using prism

_ListCons :: Prism [a] [b] (a, [a]) (b, [b])
_ListCons = prism embed match
  where
    match :: [a] -> Either [b] (a, [a])
    match [] = Left []
    match (x:xs) = Right (x, xs)
    embed :: (b, [b]) -> [b]
    embed (x, xs) = x:xs


-- “BONUS
-- 3. Implement _Cycles which detects whether a list consists of exactly ‘n’ repetitions of a pattern. It should behave as follows:

_Cycles :: (Eq a) => Int -> Prism' [a] [a]
_Cycles n = prism' embed match
  where
    match xs =
      let word = extractWord n xs
       in if (concatN n word) == xs then Just word else Nothing
    embed xs = concatN n xs
    extractWord n xs = take (length xs `div` n) xs
    concatN n w = concat $ replicate n w

-- 1. Implement the following prism and determine whether it’s lawful
_Contains :: forall a. Ord a => a -> Prism' (S.Set a) (S.Set a)
_Contains = prism' embed match
  where
    match :: a -> Maybe (S.Set a)
    match = undefined
    embed :: (S.Set a) -> a
    embed = undefined

