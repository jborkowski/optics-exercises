module IndexableStructures where

import Control.Lens
import qualified Data.Map as M
import qualified Data.Set as S

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

