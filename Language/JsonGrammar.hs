{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

module Language.JsonGrammar (
  -- * Constructing JSON grammars
  liftAeson, option, greedyOption, list, elementBy, array,
  propBy, rawFixedProp, rest, ignoreRest, object,
  
  -- * Type-directed conversion
  Json(..), fromJson, toJson, litJson, prop, fixedProp, element
  
  ) where

import Data.Iso hiding (option)

import Prelude hiding (id, (.), head, maybe, either)

import Control.Category
import Control.Monad

import Data.Aeson hiding (object)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.String
import Data.Text (Text)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Fusion.Stream as VS


aeObject :: Iso (Object :- t) (Value :- t)
aeArray  :: Iso (Array  :- t) (Value :- t)
aeNull   :: Iso            t  (Value :- t)
(aeObject, aeArray, _, _, _, aeNull) = $(deriveIsos ''Value)

-- | Convert any Aeson-enabled type to a grammar.
liftAeson :: (FromJSON a, ToJSON a) => Iso (Value :- t) (a :- t)
liftAeson = stack (Iso from to)
  where
    from = parseMaybe parseJSON
    to   = Just . toJSON

-- | Introduce 'Null' as possible value. First gives the argument grammar a
-- chance, only yielding 'Null' or 'Nothing' if the argument grammar fails to
-- handle the input.
option :: Iso (Value :- t) (a :- t) -> Iso (Value :- t) (Maybe a :- t)
option g = just . g <> nothing . inverse aeNull

-- | Introduce 'Null' as possible (greedy) value. Always converts 'Nothing' to
-- 'Null' and vice versa, even if the argument grammar knows how to handle
-- these values.
greedyOption :: Iso (Value :- t) (a :- t) -> Iso (Value :- t) (Maybe a :- t)
greedyOption g = nothing . inverse aeNull <> just . g

-- | Convert between a JSON array and Haskell list of arbitrary lengts. The
-- elements are converted using the argument grammar.
list :: Iso (Value :- t) (a :- t) -> Iso (Value :- t) ([a] :- t)
list g = duck nil >>> array (many single)
  where
    -- With ScopedTypeVariables:
    -- single :: Iso ([Value] :- [a] :- t) ([Value] :- [a] :- t)
    single = swap                -- [a] :- [Value] :- t
         >>> duck (elementBy g)  -- [a] :- [Value] :- a :- t
         >>> swap                -- [Value] :- [a] :- a :- t
         >>> duck swap           -- [Value] :- a :- [a] :- t
         >>> duck cons           -- [Value] :- [a] :- t

-- | Wrap a bunch of elements in a JSON array. For example, to match an array of exactly length two:
--
-- > array (element . element)
--
-- Or to match an empty array:
--
-- > array id
array :: Iso ([Value] :- t1) ([Value] :- t2) -> Iso (Value :- t1) t2
array els = inverse aeArray    -- Vector Value :- t1
        >>> vectorReverseList  -- [Value] :- t1
        >>> els                -- [Value] :- t2
        >>> inverse nil        -- t2

-- | Describe a single array element with the given grammar.
elementBy :: Iso (Value :- t) (a :- t) ->
  Iso ([Value] :- t) ([Value] :- a :- t)
elementBy g = inverse cons  -- Value   :- [Value] :- t
          >>> swap          -- [Value] :- Value :- t
          >>> duck g        -- [Value] :- a :- t

vectorReverseList :: Iso (V.Vector a :- t) ([a] :- t)
vectorReverseList = stack (Iso f g)
  where
    f = Just . VS.toList    . VG.streamR
    g = Just . VG.unstreamR . VS.fromList


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

-- | Collect all properties left in an object.
rest :: Iso (Object :- t) (Object :- M.Map Text Value :- t)
rest = lit M.empty

-- | Match and discard all properties left in the object. When converting back to JSON, produces no properties.
ignoreRest :: Iso (Object :- t) (Object :- t)
ignoreRest = lit M.empty . inverse (ignoreWithDefault M.empty)

-- | Wrap an exhaustive bunch of properties in an object. Typical usage:
-- 
-- > object (prop "key1" . prop "key2")
object :: Iso (Object :- t1) (Object :- t2) -> Iso (Value :- t1) t2
object props = inverse aeObject >>> props >>> inverseLit M.empty


-- Type-directed conversion

-- | Convert values of a type to and from JSON.
class Json a where
  grammar :: Iso (Value :- t) (a :- t)

instance Json a => Json [a] where
  grammar = list grammar

instance Json Bool where
  grammar = liftAeson

instance Json Int where
  grammar = liftAeson

instance Json Integer where
  grammar = liftAeson

instance Json Float where
  grammar = liftAeson

instance Json Double where
  grammar = liftAeson

instance Json [Char] where
  grammar = liftAeson

instance Json a => Json (Maybe a) where
  grammar = option grammar

instance (Json a, Json b) => Json (Either a b) where
  grammar = either grammar grammar

instance Json Value where
  grammar = id

unsafeToJson :: Json a => String -> a -> Value
unsafeToJson context value =
    fromMaybe err (convert (inverse (unstack grammar)) value)
  where
    err = error (context ++
            ": could not convert Haskell value to JSON value")

-- | Convert from JSON.
fromJson :: Json a => Value -> Maybe a
fromJson = convert (unstack grammar)

-- | Convert to JSON.
toJson :: Json a => a -> Maybe Value
toJson = convert (inverse (unstack grammar))

-- | Expect/produce a specific JSON 'Value'.
litJson :: Json a => a -> Iso (Value :- t) t
litJson = inverseLit . unsafeToJson "litJson"

-- | Describe a property whose value grammar is described by a 'Json' instance.
prop :: Json a => String -> Iso (Object :- t) (Object :- a :- t)
prop = propBy grammar

-- | Expect a specific key/value pair.
fixedProp :: Json a => String -> a -> Iso (Object :- t) (Object :- t)
fixedProp name value = rawFixedProp name (unsafeToJson "fixedProp" value)

-- | Describe a single array element whose grammar is given by a 'Json'
-- instance.
element :: Json a => Iso ([Value] :- t) ([Value] :- a :- t)
element = elementBy grammar
