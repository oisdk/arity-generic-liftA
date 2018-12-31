{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}
{-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Control.Applicative.Lift.Internal
-- Copyright   : (c) Donnacha Oisín Kidney 2018
-- License     : MIT
-- Maintainer  : mail@doisinkidney.com
-- Portability : GHC
--
-- There's a family of functions in "Control.Applicative" which follow the
-- pattern 'Control.Applicative.liftA2', 'Control.Applicative.liftA3', etc.
-- Using some tricks from Richard Eisenberg's thesis we can write them all at
-- once.
--
--     * Eisenberg, Richard A. \"Dependent Types in Haskell: Theory and
--       Practice.\" University of Pennsylvania, 2016.
--       <https://github.com/goldfirere/thesis/raw/master/built/thesis.pdf>
module Control.Applicative.Lift.Internal where

import GHC.TypeLits (Nat, type (-))

data N = Z | S N

type family FromNat (n :: Nat) :: N where
  FromNat 0 = Z
  FromNat n = S (FromNat (n - 1))

-- | @'AppFunc' f n a@ returns the type of the function @a@ "lifted" over @n@
-- arguments.
type family AppFunc f n a where
  AppFunc f Z a = f a
  AppFunc f (S n) (a -> b) = f a -> AppFunc f n b

-- | Counts the arguments of a function
type family CountArgs f where
  CountArgs (_ -> b) = S (CountArgs b)
  CountArgs _ = Z

-- | The actual class which constructs the lifted function.
class Applyable (n :: N) a where
  apply :: Applicative f => f a -> AppFunc f n a

instance {-# OVERLAPPING #-} Applyable Z a where
  apply = id
  {-# INLINE apply #-}

instance Applyable n b => Applyable (S n) (a -> b) where
  apply f x = apply @n (f <*> x)
  {-# INLINE apply #-}

-- | Lift a function over applicative arguments. This function is an
-- arity-generic version of the functions 'Control.Applicative.liftA2',
-- 'Control.Applicative.liftA3', etc.
--
-- Type inference works best when the function being lifted is
-- monomorphic:
--
-- >>> lift (\x y z -> x ++ y ++ z) (Just "a") (Just "b") (Just "c")
-- Just "abc"
--
-- In these cases, GHC can see the number of arguments the function must
-- have, and so is able to pick the correct instance for 'Applyable'.
--
-- If the function is not monomorphic (for instance '+'), you will need
-- to give a type signature:
--
-- >>> lift ((+) :: Int -> Int -> Int) (Just 1) (Just 2)
-- Just 3
--
-- Alternatively, you can use type applications to monomorphise the
-- function:
--
-- >>> :set -XTypeApplications
-- >>> lift ((+) @Int) (Just 1) (Just 2)
-- Just 3
--
-- Finally, everything is aggressively inlined, so there should be no
-- cost to using this function over manually writing
-- 'Control.Applicative.liftA3' etc.
lift :: forall f a b. (Applyable (CountArgs b) b, Applicative f) => (a -> b) -> (f a -> AppFunc f (CountArgs b) b)
lift f x = apply @(CountArgs b) (fmap f x)
{-# INLINE lift #-}

-- | A variant of 'lift' that must be explicitly applied to a type-level number,
-- but it is more flexible by allowing the function parameter to have more
-- arguments.
--
-- >>> :set -XDataKinds -XTypeApplications
-- >>> fmap ($ 10) (lift' @1 (+) (Just 1) :: Maybe (Int -> Int))
-- Just 11
lift' :: forall n f a. (Applyable (FromNat n) a, Applicative f) => a -> AppFunc f (FromNat n) a
lift' f = apply @(FromNat n) (pure @f f)
{-# INLINE lift' #-}
