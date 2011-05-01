{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module JsonGrammar where

import Iso

import Prelude hiding (id, (.), head, maybe)

import Control.Applicative
import Control.Category
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import Data.String
import Data.Text (Text)
import qualified Data.Vector as V


-- Json type class and instances

type JsonGrammar = Iso Value

class Json a where
  grammar :: Iso Value a
  grammar = unstack grammarStack
  
  grammarStack :: Iso (Value :- t) (a :- t)
  grammarStack = stack grammar

instance Json a => Json [a] where
  grammar = array grammar

instance Json Bool where
  grammar = liftAeson

instance Json Int where
  grammar = liftAeson

instance Json [Char] where
  grammar = liftAeson

instance Json a => Json (Maybe a) where
  grammar = maybe grammar

-- instance Json 

-- | Convert any Aeson-enabled type to an isomorphism.
liftAeson :: (FromJSON a, ToJSON a) => Iso Value a
liftAeson = Iso from to
  where
    from = parseMaybe parseJSON
    to   = Just . toJSON

forceToJson :: Json a => String -> a -> Value
forceToJson context value =
  case convert (inverse grammar) value of
    Just value' -> value'
    Nothing     -> error (context ++
      ": could not convert Haskell value to JSON value")

fromJson :: Json a => Value -> Maybe a
fromJson = convert grammar

toJson :: Json a => a -> Maybe Value
toJson = convert (inverse grammar)


-- Object grammars

prop :: Json a => String -> Iso (Object :- t) (Object :- a :- t)
prop = propBy grammar

propBy :: Iso Value a -> String -> Iso (Object :- t) (Object :- a :- t)
propBy g name = duck (stack g) . rawProp name

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

fixedProp :: Json a => String -> a -> Iso (Object :- t) (Object :- t)
fixedProp name value = rawFixedProp name (forceToJson "fixedProp" value)

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

object :: Iso (Object :- t1) (Object :- t2) -> Iso (Value :- t1) t2
object (Iso from to) = Iso from' to'
  where
    from' (v :- t1) = do
      Object o <- return v
      o' :- t2 <- from (o :- t1)
      guard (M.null o')  -- reject unrecognised properties
      return t2
    to' t2 = do
      o :- t1 <- to (M.empty :- t2)
      return (Object o :- t1)


-- Miscellaneous

array :: Iso Value a -> Iso Value [a]
array (Iso from to) = Iso from' to'
  where
    from' v = do
      Array vector <- return v
      mapM from (V.toList vector)
    to' xs = Array . V.fromList <$> mapM to xs

maybe :: Iso Value a -> Iso Value (Maybe a)
maybe (Iso from to) = Iso from' to'
  where
    from' Null   = return Nothing
    from' v      = Just <$> from v
    to' Nothing  = return Null
    to' (Just v) = to v

lit :: Json a => a -> Iso (Value :- t) t
lit = rawLit . forceToJson "lit"

rawLit :: Value -> Iso (Value :- t) t
rawLit value = Iso from to
  where
    from (value' :- t) = do
      guard (value == value')
      return t
    to t =
      return (value :- t)
