{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Composing where

import Control.Lens

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

-- Exercises:
waldo = view (_2 . _1 . _2) ("Ginerva", ((("Galileo", "Waldo"), "Malfoy")))

-- fiveEightDomino :: Lens' Five Eight
-- mysteryDomino   :: Lens' Eight Two
-- twoThreeDomino  :: Lens' Two Three

-- dominoTrain :: Lens' Five Three
-- dominoTrain = fiveEightDomino . mysteryDomino . twoThreeDomino

-- Functor f => (Armadillo -> f Hedgehog) -> (Platypus -> f BabySloth)

-- Lens s t a b
-- Armadillo - <a> pre-focus
-- Hedgehog - <b> post-focus 
-- Platypus - <s> pre-action
-- BabySloth - <t> post-action

-- spuzorktrowmble   ::  Lens Chumble      Spuzz      Gazork       Trowlg
-- gazorlglesnatchka ::  Lens Gazork       Trowlg     Bandersnatch Yakka
-- zinkattumblezz    ::  Lens Zink         Wattoom    Chumble      Spuzz
-- gruggazinkoom     ::  Lens Grug         Pubbawup   Zink         Wattoom
-- banderyakoobog    ::  Lens Bandersnatch Yakka      Foob         Mog
-- boowockugwup      ::  Lens Boojum       Jabberwock Grug         Pubbawup
-- snajubjumwock     ::  Lens Snark        JubJub     Boojum       Jabberwock

-- snajbufoogog :: Lens Snark JubJub Foob Mog
-- snajbufoogog = snajubjumwock . boowockugwup . gruggazinkoom . zinkattumblezz . spuzorktrowmble . gazorlglesnatchka . banderyakoobog
