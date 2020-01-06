{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE OverloadedStrings #-}
module Folds where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import Data.Char (toUpper)
import Data.Ord (comparing)

--“folding :: Foldable f => (s -> f a) -> Fold s a”
-- “folded :: Foldable f => Fold (f a) a”
-- So, we’ve got the toListOf action! It’s got an operator alias too: (^..); which is just like (^.) but, with like, one more dot… This action return a list of focuses rather than a single focus.
-- ex 1
beastSizes :: [(Int, String)]
beastSizes = [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]

res1 = beastSizes ^.. folded -- [(3, "Sirens"), (882, "Kraken"), (92, "Ogopogo")]
res2 = beastSizes ^.. folded . folded -- ["Sirens", "Kraken", "Ogopogo"]
res3 = beastSizes ^.. folded . folded . folded --  "SirensKrakenOgopogo"
res4 = beastSizes ^.. folded . _2 --  ["Sirens","Kraken","Ogopogo"]
res5 = toListOf (folded . folded) [[1, 2, 3], [4, 5, 6]] -- [1,2,3,4,5,6]
res6 = toListOf  -- "CapitanFirst Mate"
      (folded . folded)
      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])

res7 = ("Hello", "It's me") ^.. both . folded -- "HelloIt's me"
res8 = ("Why", "So", "Serious?") ^.. each -- []"Why", "So", "Serious?"]

--quotes :: [(T.Text, T.Text, T.Text)]
quotes = [("why", "So", "Serious?"), ("This", "is", "SPARTA")] 

res9 = quotes ^.. each . each . each

--ex2
res21 = toListOf (folded . _1) [(1, 'a'), (2, 'b'), (3, 'c')]
-- [1, 2, 3]
--folded :: Fold [(Int, Char)] Int
-- _1 :: Fold (Int, Char) Int

res22 = toListOf (_2 . folded) (False, S.fromList ["one", "two", "three"])
-- ["one", "two", "three"]
-- _2 :: Fold (S.Set [String]) String 
-- folded :: Fold (S.Set String) String

res23 = toListOf 
      (folded . folded)
      (M.fromList [("Jack", "Captain"), ("Will", "First Mate")])
-- "CaptainFirst Mate”
-- folded :: Fold (M.Map String String) String
-- folded :: Fold String Char

-- ex 3
res31 = [1,2,3] ^.. folded
res32 = ("Light", "Light") ^.. _1
res33 = [("Light", "Dark"), ("Happy", "Sad")] ^.. each . each
res34 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . _1
res35 = [("Light", "Dark"), ("Happy", "Sad")] ^.. folded . folded . folded
res36 = ("Bond", "James", "Bond") ^.. each


newtype Name = Name
    { getName :: String
    } deriving Show

data ShipCrew = ShipCrew
    { _shipName :: Name
    , _captain    :: Name
    , _firstMate  :: Name
    , _conscripts :: [Name]
    } deriving (Show)
makeLenses ''ShipCrew


collectCrewMembers :: ShipCrew -> [Name]
collectCrewMembers crew =
  [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers :: Fold ShipCrew Name
crewMembers = folding collectCrewMembers

myCrew :: ShipCrew
myCrew =
  ShipCrew
  { _shipName   = Name "Purple Pearl"
  , _captain    = Name "Grumpy Roger"
  , _firstMate  = Name "Long-John Bronze"
  , _conscripts = [Name "one-eyed Jack", Name "Filthy Frank"]
  }

-- >>> myCrew ^.. crewMembers
-- res: [Name {getName = "Grumpy Roger"},Name {getName = "Long-John Bronze"},Name {getName = "one-eyed Jack"},Name {getName = "Filthy Frank"}]

-- Mapping over folds
-- to :: (s -> a) -> Fold s a

ex11 = Name "Two-faced Tony" ^. to getName -- "Two-faced Tony"

-- We can chain many `to`s in a row
ex12 = Name "Two-faced Tony" ^. to getName . to (fmap toUpper) --"TWO-FACED TONY"

-- Or simply use function composition before passing to `to`
-- However, I find it confusing to switch from reading
-- left-to-right into right-to-left like this:
ex13 = Name "Two-faced Tony" ^. to (fmap toUpper . getName) -- "TWO-FACED TONY"

-- Multiple Folds
crewNames :: Fold ShipCrew Name
crewNames =
  folding (\s -> s ^.. captain
              <> s ^.. firstMate
              <> s ^.. conscripts . folded)

ex21 = myCrew ^.. crewNames . to getName -- ["Grumpy Roger","Long-John Bronze","one-eyed Jack","Filthy Frank"]

-- Excerscises
res41 = ["Yer", "a", "wizard", "Harry"] ^.. folded . folded -- "YerawizardHarry" 
res42 = [[1, 2, 3], [4, 5, 6]] ^.. folded . folding (take 2) -- [1,2,4,5]
res43 = [[1, 2, 3], [4, 5, 6]] ^.. folded . to (take 2) -- [[1,2],[4,5]]
res44 = ["bob", "otto", "hannah"] ^.. folded . to reverse -- ["bob","otto","hannah"]
res45 = ("abc", "def") ^.. folding (\(a,b) -> [a,b]) . to reverse . folded -- "cbafed"

res51 = [1..5] ^.. folded . to (*100) -- [100,200,300,400,500]
res52 = (1,2) ^.. both -- [1,2]
res53 = [(1, "one"), (2, "two")] ^.. folded . to snd -- ["one","two"]
res54 = (Just 1, Just 2, Just 3) ^.. folding (\(a, b, c) -> [a,b,c]) . folded -- [1,2,3]
res55 = [Left 1, Right 2, Left 3] ^.. folded . folded -- [2]
res56 = [([1, 2], [3, 4]), ([5, 6], [7, 8])] ^.. folded . folding (\(a,b) -> [a,b]) . folded -- [1,2,3,4,5,6,7,8]
res57 = [1, 2, 3, 4] ^.. folded . to (\a -> if a `mod` 2 == 1 then Left a else Right a)
res58 = [(1, (2, 3)), (4, (5, 6))] ^.. folded . folding (\(a,(b,c)) -> [a,b,c]) -- [1,2,3,4,5,6]
res59 = [(Just 1, Left "one"), (Nothing, Right 2)] ^.. folded . folding (\(a,b) -> a ^.. folded <> b ^.. folded) 
res60 = [(1, "one"), (2, "tow")] ^.. folded . folding (\(a,b) -> [Left a, Right b])
res61 = S.fromList ["apricots",  "apples"] ^.. folded . folding reverse
res62 =  [(12, 45, 66), (91, 123, 87)] ^.. folded . _2 . to show . to reverse . folded -- "54321"
res63 =[(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(a,b) -> if odd a then Nothing else Just b) -- ["b","d"]



-- Exercise: Fold Actions
res70 = has folded []
res71 = foldOf both ("Yo", "Adrian!")
res72 = elemOf both "phone" ("E.T.", "phone", "home")
res73 = minimumOf folded [5,7,2,3,13,16,11]
res74 = lastOf folded [5,7,2,3,13,16,11]
res75 = allOf folded ((>9) . length) ["Bulbasaur", "Charmander", "Squirtle"]
res76 = findOf folded even [11,22,3,5,6]

res80 = findOf folded isPalindrome ["umbrella", "olives", "racecar", "hammer"]

isPalindrome :: String -> Bool
isPalindrome w = w == reverse w

res81 = allOf each even (2, 4, 6)
res82 = maximumByOf folded (comparing fst) [(2, "I'll"), (3, "Be"), (1, "Back")]
res83 = sumOf each (1,2)

res84 = maximumByOf (folding words) (comparing (length . filter isVowel)) "Do or do not, there is no try."

isVowel :: Char -> Bool
isVowel c = c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u' || c == 'y'

res85 = foldBy (flip (++)) "" ["a", "b", "c"]

res86 = [(12, 45, 66), (91, 123, 87)] ^.. folded ._2 . to show .to reverse . folded

--very interesting behavior without *return* 
res87 = [(1, "a"), (2, "b"), (3, "c"), (4, "d")] ^.. folded . folding (\(a,b) -> if even a then return b else [])
