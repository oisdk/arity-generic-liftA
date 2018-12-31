-- |
-- Module      : Control.Applicative.Lift
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
--
-- This module exports the one 'lift' function; for the internal
-- implementation details see "Control.Applicative.Lift.Internal".

module Control.Applicative.Lift
  (lift)
  where

import           Control.Applicative.Lift.Internal (lift)
