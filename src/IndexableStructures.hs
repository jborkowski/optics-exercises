{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
module IndexableStructures where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

-- Fill in the blanks
ex11 = ["Larry", "Curly", "Moe"] & ix 1 .~ "Wiggly"
-- Res ex11: ["Larry", "Wiggly", "Moe"]

herosAndVillains = M.fromList [ ("Superman", "Lex"), ("Batman", "Joker") ]
ex12 = herosAndVillains & at "Spiderman" .~ Just "Goblin"
-- Res ex12: M.fromList [ ("Superman", "Lex"), ("Batman", "Joker"), ("Spiderman", "Goblin") ]

ex13 = sans "Superman" herosAndVillains
-- Res ex13: M.fromList [("Batman", "Joker")]

ex14 = S.fromList ['a', 'e', 'i', 'o', 'u']
  & at 'y' ?~ ()
  & at 'i' .~ Nothing
-- Res ex14: S.fromList "aeouy"


-- Custom Indexed Structures -
newtype InsensitiveCase v = InsensitiveCase
  { _unInsensitiveCase :: M.Map T.Text v
  } deriving Show

makeLenses ''InsensitiveCase

type instance Index (InsensitiveCase v) = T.Text
type instance IxValue (InsensitiveCase v) = v

instance Ixed (InsensitiveCase v) where
  ix :: T.Text -> Traversal' (InsensitiveCase v) v
  ix k = unInsensitiveCase . ix (T.toLower k)

instance At (InsensitiveCase v) where
  at :: T.Text -> Lens' (InsensitiveCase v) (Maybe v)
  at k = unInsensitiveCase . at (T.toLower k)

-- Missing Values

-- Write an optic which focuses the value at key "first" or failing that, the value at key "second"

optic = ix "first" `failing` ix "second"
ex21 = M.fromList [("first", False), ("second", False)] & optic .~ True
-- Res ex23: fromList [("first", True), ("second", False)]

ex22 = M.fromList [("second", False)] & optic .~ True
-- Res ex22: fromList [("second", True)]

-- Write an optics which focuses the first element of a tuple if it is even, add the second tuple element otherwise. Assume each slot contains an integer.

optic1 = _1 . filtered even `failing` _2
ex31 = (1, 1) & optic1 *~ 10
-- Res ex31: (1,10)

ex32 = (2, 2) & optic1 *~ 10
-- Res ex32: (20, 2)

-- Write an optic which focuses all even numbers in a list, if none of the members are odd then focuses ALL numbers in the list.
optic2 = folded . filtered even `failing` folded
ex41 = [1, 2, 3, 4] ^.. optic2
-- Res ex41: [2,4]

ex42 = [1, 3, 5] ^.. optic2
-- Res ex42: [1, 3, 5]


-- Fill in the blanks

ex51 = Nothing ^. non "default"
-- Res ex51: "default"

ex52 = Nothing  & non 100 +~ 7
-- Res ex52: Just 107

ex53 = M.fromList [("Perogird", True), ("Pizza", True), ("Pilsners", True)] ^. at "Broccoli" . non False
-- Res ex53: False

ex54 = M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000)] & at "Wario's Woods" . non 0 +~ 999
-- Res ex54: M.fromList [("Breath of the wild", 22000000), ("Odyssey", 9070000), ("Wario's Woods", 999)]

ex55 = ["Math", "Science", "Geography"] ^. pre (ix 4) . non "Unscheduled"
-- Res ex55: "Unscheduled"

-- Bonus
-- Use 'pre' and 'non'
ex61 = [1,2,3,4] ^.. traversed . pre (filtered even) . non (-1)
-- Res ex61: [-1,2,-1,4]
