{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module VirtualFields where

import Control.Lens
import Control.Lens.Unsound
import Control.Applicative
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T

data Temperature =
    Temperature { _location :: String
                , _kelvin  :: Float
                }
    deriving (Show)
makeLenses ''Temperature

celsius :: Lens' Temperature Float
celsius = lens getter setter
  where
    getter = (subtract 273.15) . view kelvin
    setter temp c = set kelvin (c + 273.15) temp

celsiusToFahrenheit :: Float -> Float
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius f = (f - 32) * (5/9)

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = set celsius (fahrenheitToCelsius f) temp

readTemp :: IO Float
readTemp = return 13.2

updateTempReading :: Temperature -> IO Temperature
updateTempReading temp = do
  newTempInCelsius <- readTemp
  return $ set celsius newTempInCelsius temp


data User =
  User { _firstName :: String
       , _lastName :: String
       , _email :: String
       }
  deriving Show

makeLenses ''User

username :: Lens' User String
username = email
 

fullName :: Lens' User String
fullName = lens getter setter
  where
    getter user = view firstName user <> " " <> view lastName user
    setter user new =
      let withFirstName = set firstName (head (words new)) user
      in set lastName (unwords . tail . words $ new) withFirstName

data ProducePrices =
    ProducePrices { _limePrice  :: Float
                 , _lemonPrice :: Float
                 }
    deriving Show

limePrice :: Lens' ProducePrices Float
limePrice = lens getter setter
  where
    getter = _limePrice
    setter produces price = produces{_limePrice = max 0 price}

lemonPrice :: Lens' ProducePrices Float
lemonPrice = lens getter setter
  where
    getter = _lemonPrice
    setter produces price =  produces{_lemonPrice = max 0 price}
