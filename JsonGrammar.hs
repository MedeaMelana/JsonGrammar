{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module JsonGrammar where

import Iso

import Prelude hiding (id, (.), head)

import Control.Applicative
import Control.Category

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Vector as V

import Control.Monad


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


-- Object grammars

prop :: Json a => Text -> Iso (Object :- t) (Object :- a :- t)
prop = propBy grammar

propBy :: Iso Value a -> Text -> Iso (Object :- t) (Object :- a :- t)
propBy g name = duck (stack g) . rawProp name

rawProp :: Text -> Iso (Object :- t) (Object :- Value :- t)
rawProp name = Iso from to
  where
    from (o :- r) = do
      value <- M.lookup name o
      return (M.delete name o :- value :- r)
    to (o :- value :- r) = do
      guard (M.notMember name o)
      return (M.insert name value o :- r)

fixedProp :: Json a => Text -> a -> Iso (Object :- t) (Object :- t)
fixedProp name value = rawFixedProp name (forceToJson "fixedProp" value)

rawFixedProp :: Text -> Value -> Iso (Object :- t) (Object :- t)
rawFixedProp name value = stack (Iso from to)
  where
    from o = do
      value' <- M.lookup name o
      guard (value' == value)
      return (M.delete name o)
    to o = do
      guard (M.notMember name o)
      return (M.insert name value o)

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


-- Arrays

array :: Iso Value a -> Iso Value [a]
array (Iso from to) = Iso from' to'
  where
    from' v = do
      Array vector <- return v
      mapM from (V.toList vector)
    to' xs = Array . V.fromList <$> mapM to xs


-- Literals

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
