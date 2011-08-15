{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonoPatBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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


unit :: (Monad m, Monad n) => Iso m n t (() :- t)
unit = Iso f g
  where
    f = arr (() :-)
    g = arr (\(_ :- t) -> t)

tup :: (Monad m, Monad n) => Iso m n (a :- b :- t) ((a, b) :- t)
tup = Iso f g
  where
    f = arr $ \(a :- b :- t) -> ((a, b) :- t)
    g = arr $ \((a, b) :- t) -> (a :- b :- t)

tup3 :: (Monad m, Monad n) => Iso m n (a :- b :- c :- t) ((a, b, c) :- t)
tup3 = Iso f g
  where
    f = arr $ \(a :- b :- c :- t) -> ((a, b, c)   :- t)
    g = arr $ \((a, b, c)   :- t) -> (a :- b :- c :- t)

nothing :: (Monad m, Monad n) => Iso m n t (Maybe a :- t)
just    :: (Monad m, Monad n) => Iso m n (a :- t) (Maybe a :- t)
(nothing, just) = $(deriveIsos ''Maybe)

maybe :: (MonadPlus m, MonadPlus n) =>
  Iso m n t (a :- t) -> Iso m n t (Maybe a :- t)
maybe el = just . el <> nothing


nil :: (Monad m, MonadPlus n) => Iso m n t ([a] :- t)
nil = Iso f g
  where
    f = arr ([] :-)
    g = Kleisli $ \(xs :- t) ->
          case xs of
            [] -> return t
            _  -> mzero

cons :: (Monad m, MonadPlus n) => Iso m n (a :- [a] :- t) ([a] :- t)
cons = Iso f g
  where
    f = Kleisli $ \(x :- xs  :- t) -> return ((x : xs) :- t)
    g = Kleisli $ \(xs' :- t) ->
          case xs' of
            x : xs -> return (x :- xs :- t)
            _      -> mzero


left  :: Iso m n (a :- t) (Either a b :- t)
right :: Iso m n (b :- t) (Either a b :- t)
(left, right) = $(deriveIsos ''Either)

either :: (MonadPlus m, MonadPlus n) => Iso m n t1 (a :- t2) ->
  Iso m n t1 (b :- t2) -> Iso m n t1 (Either a b :- t2)
either f g = left . f <> right . g


false :: (Monad m, MonadPlus n) => Iso m n t (Bool :- t)
true  :: (Monad m, MonadPlus n) => Iso m n t (Bool :- t)
(false, true) = $(deriveIsos ''Bool)

bool  :: (MonadPlus m, MonadPlus n) => Iso m n t (Bool :- t)
bool = false <> true
