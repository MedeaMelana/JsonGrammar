{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}

-- | Constructor-destructor isomorphisms for some common datatypes.
module Data.Iso.Common (

  -- * @()@
  unit,
  
  -- * @(,)@
  tup,
  
  -- * @(,,)@
  tup3,

  -- * @Maybe a@
  nothing, just, maybe,
  
  -- * @[a]@
  nil, cons,
  
  -- * @Either a b@
  left, right, either,
  
  -- * @Bool@
  false, true, bool

  ) where

import Prelude hiding (id, (.), maybe, either)
import Control.Category
import Control.Arrow (arr, Kleisli(..))
import Control.Monad

import Data.Iso.Core
import Data.Iso.TH

import Data.Semigroup


unit :: Iso t (() :- t)
unit = Iso f g
  where
    f = arr (() :-)
    g = arr (\(_ :- t) -> t)

tup :: Iso (a :- b :- t) ((a, b) :- t)
tup = Iso f g
  where
    f = arr $ \(a :- b :- t) -> ((a, b) :- t)
    g = arr $ \((a, b) :- t) -> (a :- b :- t)

tup3 :: Iso (a :- b :- c :- t) ((a, b, c) :- t)
tup3 = Iso f g
  where
    f = arr $ \(a :- b :- c :- t) -> ((a, b, c)   :- t)
    g = arr $ \((a, b, c)   :- t) -> (a :- b :- c :- t)

nothing :: Iso t (Maybe a :- t)
just    :: Iso (a :- t) (Maybe a :- t)
(nothing, just) = $(deriveIsos ''Maybe)

maybe :: Iso t (a :- t) -> Iso t (Maybe a :- t)
maybe el = just . el <> nothing


nil :: Iso t ([a] :- t)
nil = Iso f g
  where
    f = arr ([] :-)
    g = Kleisli $ \(xs :- t) ->
          case xs of
            [] -> return t
            _  -> mzero

cons :: Iso (a :- [a] :- t) ([a] :- t)
cons = Iso f g
  where
    f = Kleisli $ \(x :- xs  :- t) -> return ((x : xs) :- t)
    g = Kleisli $ \(xs' :- t) ->
          case xs' of
            x : xs -> return (x :- xs :- t)
            _      -> mzero


left  :: Iso (a :- t) (Either a b :- t)
right :: Iso (b :- t) (Either a b :- t)
(left, right) = $(deriveIsos ''Either)

either :: Iso t1 (a :- t2) -> Iso t1 (b :- t2) -> Iso t1 (Either a b :- t2)
either f g = left . f <> right . g


false :: Iso t (Bool :- t)
true  :: Iso t (Bool :- t)
(false, true) = $(deriveIsos ''Bool)

bool  :: Iso t (Bool :- t)
bool = false <> true
