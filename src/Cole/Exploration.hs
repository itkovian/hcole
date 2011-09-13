{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
 - (C) 2011 Andy Georges
 -
 - Module that implements the actual COLE exploration.
 -
 -}

module Cole.Exploration
    (
    ) where

import           Control.Monad (liftM)

import qualified GA as GA

import           Cole.Entity

instance (Monad m, Entity m a) => GA.Mutator a where
    -- |implementation of the crossover operator
    crossover p seed e1 e2 = liftM fst $ crossoverOnePoint p seed e1 e2 

    -- |implementation of the mutation operator
    mutation p seed e = mutationMultiPointDrift (p,p) seed e

