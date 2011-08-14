{-# LANGUAGE TypeOperators #-}

module Data.Iso.Core (

  -- * Partial isomorphisms
  Iso(..), convert, inverse, many,
  
  -- * Stack-based isomorphisms
  (:-)(..), stack, unstack, swap, duck,
  lit, inverseLit, matchWithDefault, ignoreWithDefault
  
  ) where


import Prelude hiding (id, (.), head)

import Data.Monoid
import Data.Semigroup

import Control.Applicative hiding (many)
import Control.Arrow
import Control.Monad
import Control.Category


-- Partial isomorphisms

-- | Bidirectional partial isomorphism.
data Iso a b = Iso (Kleisli Maybe a b) (Kleisli Maybe b a)

instance Category Iso where
  id                          = Iso id id
  ~(Iso f1 g1) . ~(Iso f2 g2) = Iso (f1 . f2) (g2 . g1)

instance Monoid (Iso a b) where
  mempty = Iso zeroArrow zeroArrow
  ~(Iso f1 g1) `mappend` ~(Iso f2 g2) =
    Iso (f1 <+> f2) (g1 <+> g2)

instance Semigroup (Iso a b) where
  (<>) = mappend

-- | Apply an isomorphism in one direction.
convert :: Iso a b -> a -> Maybe b
convert (Iso f _) = runKleisli f

-- | Inverse of an isomorphism.
inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

-- | Apply an isomorphism as many times as possible, greedily.
many :: Iso a a -> Iso a a
many (Iso f g) = Iso manyF manyG
  where
    manyF = manyF . f <+> id
    manyG = manyG . g <+> id


-- Stack-based isomorphisms

-- | Heterogenous stack with a head and a tail.
data h :- t = h :- t
  deriving (Eq, Show)
infixr 5 :-

head :: (h :- t) -> h
head (h :- _) = h

-- | Convert to a stack isomorphism.
stack :: Iso a b -> Iso (a :- t) (b :- t)
stack (Iso f g) = Iso (lift f) (lift g)
  where
    lift (Kleisli k) = Kleisli $ \(x :- t) -> (:- t) <$> k x

-- | Convert from a stack isomorphism.
unstack :: Iso (a :- ()) (b :- ()) -> Iso a b
unstack (Iso f g) = Iso (lift f) (lift g)
  where
    lift (Kleisli k) = Kleisli $ fmap head . k . (:- ())

-- | Swap the top two arguments.
swap :: Iso (a :- b :- t) (b :- a :- t)
swap = Iso f f
  where
    f = arr $ \(x :- y :- t) -> (y :- x :- t)

-- | Introduce a head value that is passed unmodified.
duck :: Iso t1 t2 -> Iso (h :- t1) (h :- t2)
duck (Iso f g) = Iso (lift f) (lift g)
  where
    lift (Kleisli k) = Kleisli $ \(h :- t) -> (h :-) <$> k t

-- | Push or pop a specific value.
lit :: Eq a => a -> Iso t (a :- t)
lit x = Iso f g
  where
    f = arr (x :-)
    g = Kleisli $ \(x' :- t) -> do
      guard (x' == x)
      return t

-- | Inverse of 'lit'.
inverseLit :: Eq a => a -> Iso (a :- t) t
inverseLit = inverse . lit

-- | When converting from left to right, push the default value on top of the
-- stack. When converting from right to left, pop the value, make sure it
-- matches the predicate and then discard it.
matchWithDefault :: (a -> Bool) -> a -> Iso t (a :- t)
matchWithDefault p x = Iso f g
  where
    f = arr (x :-)
    g = Kleisli $ \(x' :- t) -> do
      guard (p x')
      return t

-- | When converting from left to right, push the default value on top of the stack. When converting from right to left, pop the value and discard it.
ignoreWithDefault :: a -> Iso t (a :- t)
ignoreWithDefault = matchWithDefault (const True)
