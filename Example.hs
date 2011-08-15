{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Example where

import Data.Iso
import Language.JsonGrammar

import Prelude hiding (id, (.), head, either)
import Control.Category

import Data.Aeson (Object)


data Person = Person
  { name     :: String
  , gender   :: Gender
  , age      :: Int
  -- , lat      :: Float
  -- , lng      :: Float
  , location :: Coords
  } deriving (Eq, Show)

data Gender = Male | Female
  deriving (Eq, Show)

data Coords = Coords { lat :: Float, lng :: Float }
  deriving (Eq, Show)

person         = $(deriveIsos ''Person)
(male, female) = $(deriveIsos ''Gender)
coords         = $(deriveIsos ''Coords)


instance Json Person where
  grammar = person . object
    ( prop "naam"
    . prop "geslacht"
    . prop "leeftijd"
    . coordsProps
    )

instance Json Gender where
  grammar = male   . litJson "man"
         <> female . litJson "vrouw"

coordsProps :: Grammar (Object :- t) (Object :- Coords :- t)
coordsProps = duck coords . prop "lat" . prop "lng"

anna :: Person
anna = Person "Anna" Female 36 (Coords 53.0163038 5.1993053)
