{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}

{-# OPTIONS_GHC -fno-warn-unticked-promoted-constructors #-}

module Control.Applicative.Lift.Internal where

data N = Z | S N

type family AppFunc f n a where
  AppFunc f Z a = f a
  AppFunc f (S n) (a -> b) = f a -> AppFunc f n b

type family CountArgs f where
  CountArgs (_ -> b) = S (CountArgs b)
  CountArgs _ = Z

class (CountArgs a ~ n) => Applyable a n where
  apply :: Applicative f => f a -> AppFunc f (CountArgs a) a

instance (CountArgs a ~ Z) => Applyable a Z where
  apply = id
  {-# INLINE apply #-}

instance Applyable b n => Applyable (a -> b) (S n) where
  apply f x = apply (f <*> x)
  {-# INLINE apply #-}

-- | >>> lift (\x y z -> x ++ y ++ z) (Just "a") (Just "b") (Just "c")
-- Just "abc"
lift :: (Applyable a n, Applicative f) => (b -> a) -> (f b -> AppFunc f n a)
lift f x = apply (fmap f x)
{-# INLINE lift #-}
