{-# LANGUAGE TemplateHaskell #-}
module Prism where

import Control.Lens

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





