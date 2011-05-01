{-# LANGUAGE TypeOperators #-}

module Iso where


import Prelude hiding (id, (.), head)

import Data.Monoid

import Control.Applicative
import Control.Monad
import Control.Category


-- Partial isomorphisms

data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

instance Category Iso where
  id                    = Iso Just Just
  Iso f1 g1 . Iso f2 g2 = Iso (f1 <=< f2) (g1 >=> g2)

instance Monoid (Iso a b) where
  mempty = Iso (const Nothing) (const Nothing)
  Iso f1 g1 `mappend` Iso f2 g2 =
    Iso
      ((<|>) <$> f1 <*> f2)
      ((<|>) <$> g1 <*> g2)

infixl 3 <>
(<>) :: Monoid a => a -> a -> a
(<>) = mappend

convert :: Iso a b -> a -> Maybe b
convert (Iso f _) x = f x

inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f


-- Stack-based isomorphisms

data a :- b = a :- b
infixr 5 :-

head :: (h :- t) -> h
head (h :- _) = h

stack :: Iso a b -> Iso (a :- t) (b :- t)
stack (Iso f g) = Iso (lift f) (lift g)
  where
    lift k (x :- t) = (:- t) <$> k x

unstack :: Iso (a :- ()) (b :- ()) -> Iso a b
unstack (Iso f g) = Iso (lift f) (lift g)
  where
    lift k = fmap head . k . (:- ())

duck :: Iso t1 t2 -> Iso (h :- t1) (h :- t2)
duck (Iso f g) = Iso (lift f) (lift g)
  where
    lift k (h :- t) = (h :-) <$> k t
