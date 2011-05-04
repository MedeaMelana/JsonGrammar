{-# LANGUAGE TypeOperators #-}

module Data.Iso.Core (

  -- * Partial isomorphisms
  Iso(..), convert, inverse, (<>),
  
  -- * Stack-based isomorphisms
  (:-)(..), stack, unstack, duck, lit, inverseLit
  
  ) where


import Prelude hiding (id, (.), head)

import Data.Monoid

import Control.Applicative
import Control.Monad
import Control.Category


-- Partial isomorphisms

-- | Bidirectional partial isomorphism.
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

-- | Apply an isomorphism in one direction.
convert :: Iso a b -> a -> Maybe b
convert (Iso f _) = f

-- | Inverse of an isomorphism.
inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f


-- Stack-based isomorphisms

-- | Heterogenous stack with a head and a tail.
data h :- t = h :- t
infixr 5 :-

head :: (h :- t) -> h
head (h :- _) = h

-- | Convert to a stack isomorphism.
stack :: Iso a b -> Iso (a :- t) (b :- t)
stack (Iso f g) = Iso (lift f) (lift g)
  where
    lift k (x :- t) = (:- t) <$> k x

-- | Convert from a stack isomorphism.
unstack :: Iso (a :- ()) (b :- ()) -> Iso a b
unstack (Iso f g) = Iso (lift f) (lift g)
  where
    lift k = fmap head . k . (:- ())

-- | Swap the top two arguments.
swap :: Iso (a :- b :- t) (b :- a :- t)
swap = Iso f f
  where
    f (x :- y :- t) = Just (y :- x :- t)

-- | Introduce a head value that is passed unmodified.
duck :: Iso t1 t2 -> Iso (h :- t1) (h :- t2)
duck (Iso f g) = Iso (lift f) (lift g)
  where
    lift k (h :- t) = (h :-) <$> k t

-- | Push or pop a specific value.
lit :: Eq a => a -> Iso t (a :- t)
lit x = Iso f g
  where
    f t = Just (x :- t)
    g (x' :- t) = do
      guard (x' == x)
      Just t

-- | Inverse of 'lit'.
inverseLit :: Eq a => a -> Iso (a :- t) t
inverseLit = inverse . lit
