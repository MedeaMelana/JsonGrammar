{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Language.JsonGrammar (
  -- * The Json type class
  Json(..), fromJson, toJson,
  
  -- * Constructing JSON grammars
  option, array,
  liftAeson, prop, propBy, object, fixedProp, rawFixedProp, litJson
  
  ) where

import Data.Iso.Core
import Data.Iso.TH
import Data.Iso.Common

import Prelude hiding (id, (.), head, maybe)

import Control.Applicative
import Control.Category
import Control.Monad

import Data.Aeson hiding (object)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String
import qualified Data.Vector as V


(aeObject, aeArray, _, _, _, aeNull) = $(deriveIsos ''Value)


-- Json type class and instances

-- | Convert values of a type to and from JSON. Minimal complete definition: either 'grammar' or 'grammarStack'.
class Json a where
  grammar :: Iso Value a
  grammar = unstack grammarStack
  
  grammarStack :: Iso (Value :- t) (a :- t)
  grammarStack = stack grammar

instance Json a => Json [a] where
  grammarStack = array grammarStack

instance Json Bool where
  grammar = liftAeson

instance Json Int where
  grammar = liftAeson

instance Json [Char] where
  grammar = liftAeson

instance Json a => Json (Maybe a) where
  grammarStack = option grammarStack

-- instance Json 

-- | Convert any Aeson-enabled type to a grammar.
liftAeson :: (FromJSON a, ToJSON a) => Iso Value a
liftAeson = Iso from to
  where
    from = parseMaybe parseJSON
    to   = Just . toJSON

forceToJson :: Json a => String -> a -> Value
forceToJson context value =
    fromMaybe err (convert (inverse grammar) value)
  where
    err = error (context ++
            ": could not convert Haskell value to JSON value")

-- | Convert from JSON.
fromJson :: Json a => Value -> Maybe a
fromJson = convert grammar

-- | Convert to JSON.
toJson :: Json a => a -> Maybe Value
toJson = convert (inverse grammar)


-- Object grammars

-- | Describe a property whose value grammar is described by a 'Json' instance.
prop :: Json a => String -> Iso (Object :- t) (Object :- a :- t)
prop = propBy grammarStack

-- | Describe a property with the given name and value grammar.
propBy :: Iso (Value :- t) (a :- t) -> String -> Iso (Object :- t) (Object :- a :- t)
propBy g name = duck g . rawProp name

rawProp :: String -> Iso (Object :- t) (Object :- Value :- t)
rawProp name = Iso from to
  where
    textName = fromString name
    from (o :- r) = do
      value <- M.lookup textName o
      return (M.delete textName o :- value :- r)
    to (o :- value :- r) = do
      guard (M.notMember textName o)
      return (M.insert textName value o :- r)

-- | Expect a specific key/value pair.
fixedProp :: Json a => String -> a -> Iso (Object :- t) (Object :- t)
fixedProp name value = rawFixedProp name (forceToJson "fixedProp" value)

-- | Expect a specific key/value pair.
rawFixedProp :: String -> Value -> Iso (Object :- t) (Object :- t)
rawFixedProp name value = stack (Iso from to)
  where
    textName = fromString name
    from o = do
      value' <- M.lookup textName o
      guard (value' == value)
      return (M.delete textName o)
    to o = do
      guard (M.notMember textName o)
      return (M.insert textName value o)

-- | Wrap an exhaustive bunch of properties in an object. Typical usage:
-- 
-- > object (prop "key1" . prop "key2")
object :: Iso (Object :- t1) (Object :- t2) -> Iso (Value :- t1) t2
object props = inverse aeObject >>> props >>> inverseLit (M.empty)

-- | Describe an array whose elements match the given grammar.
array :: Iso (Value :- ()) (a :- ()) -> Iso (Value :- t) ([a] :- t)
array = stack . array' . unstack
  where
    array' :: Iso Value a -> Iso Value [a]
    array' (Iso from to) = Iso from' to'
      where
        from' v = do
          Array vector <- return v
          mapM from (V.toList vector)
        to' xs = Array . V.fromList <$> mapM to xs

-- TODO: Write array in terms of stack operations
-- array :: forall t a. Iso (Value :- t) (a :- t) -> Iso (Value :- t) ([a] :- t)
-- array g = inverse aeArray >>> vectorList >>> elements
--   where
--     elements :: Iso ([Value] :- t) ([a] :- t)
--     elements = undefined
-- 
-- vectorList :: Iso (V.Vector a :- t) ([a] :- t)
-- vectorList = stack (Iso f g)
--   where
--     f = Just . V.toList
--     g = Just . V.fromList


-- | Introduce 'Null' as possible (greedy) value. Always converts 'Nothing' to
-- 'Null' and vice versa, even if the argument grammar knows how to handle
-- these values.
greedyOption :: Iso (Value :- t) (a :- t) -> Iso (Value :- t) (Maybe a :- t)
greedyOption g = nothing . inverse aeNull <> just . g

-- | Introduce 'Null' as possible value. First gives the argument grammar a
-- chance, only yielding 'Null' or 'Nothing' if the argument grammar fails to
-- handle the input.
option :: Iso (Value :- t) (a :- t) -> Iso (Value :- t) (Maybe a :- t)
option g = just . g <> nothing . inverse aeNull

-- | Expect/produce a specific JSON 'Value'.
litJson :: Json a => a -> Iso (Value :- t) t
litJson = inverseLit . forceToJson "litJson"
