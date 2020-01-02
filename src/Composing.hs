{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Composing where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Person =
  Person { _name :: String
         , _address :: Address
         }
  deriving Show

data Address =
  Address { _streetAddress :: StreetAddress
          , _city :: String
          , _country :: String
          }
  deriving Show

data StreetAddress =
  StreetAddress { _streetNumber :: String
                , _streetName :: String
                }
  deriving Show

makeLenses ''Person
makeLenses ''Address
makeLenses ''StreetAddress

sherlock :: Person
sherlock =
  Person { _name = "S. Holmes"
         , _address = Address
           { _streetAddress = StreetAddress { _streetNumber = "221B"
                                            , _streetName = "Baker Street"
                                            }
           , _city = "London"
           , _country = "England"
           }
         }

-- Lenses way update
updateAddress :: (Address -> Address)
              -> (Person -> Person)
updateAddress modify existingPerson =
  existingPerson
  { _address = modify . _address $ existingPerson
  }

updateStreetAddress :: (StreetAddress -> StreetAddress)
                    -> (Address -> Address)
updateStreetAddress modify existingAddress =
  existingAddress
  { _streetAddress = modify . _streetAddress $ existingAddress
  }

updateStreetNumber :: (String -> String)
                   -> (StreetAddress -> StreetAddress)
updateStreetNumber modify existingStreetNumber =
  existingStreetNumber
  { _streetNumber = modify . _streetNumber $ existingStreetNumber
  }


  
movedSherlock =
  (updateAddress . updateStreetAddress . updateStreetNumber) (const "221A") sherlock


-- Important
-- Modifier - every function which accepts and returns the same type
-- Updater  - function accepts and returns modifiers
