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

