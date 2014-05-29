{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

import Types

import Data.Iso
import Language.JsonGrammar

import Prelude hiding (id, (.), head, either)
import Control.Category

import Data.Aeson (Object)
import Test.Framework (Test, defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual)


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

coordsProps :: Iso (Object :- t) (Object :- Coords :- t)
coordsProps = duck coords . prop "lat" . prop "lng"

anna :: Person
anna = Person "Anna" Female 36 (Coords 53.0163038 5.1993053)

main :: IO ()
main = defaultMain [personTest]

personTest :: Test
personTest = testCase "Person" (assertEqual "" anna anna')
  where
    Just anna' = fromJson annaJson
    Just annaJson = toJson anna
