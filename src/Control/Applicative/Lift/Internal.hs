{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

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

-- | Simple implementation of Peano numbers.
data N = Z | S N

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
class (CountArgs a ~ n) => Applyable a n where
  apply :: Applicative f => f a -> AppFunc f (CountArgs a) a

instance (CountArgs a ~ Z) => Applyable a Z where
  apply = id
  {-# INLINE apply #-}

instance Applyable b n => Applyable (a -> b) (S n) where
  apply f x = apply (f <*> x)
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
lift :: (Applyable b n, Applicative f) => (a -> b) -> (f a -> AppFunc f n b)
lift f x = apply (fmap f x)
{-# INLINE lift #-}
