-----------------------------------------------------------------------------
--
-- Module      :  Spea2.Entity
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  Andy Georges
-- Stability   :  unstable
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module Spea2.Entity
    ( Entity
    ) where


class (Show a, Eq a) => Entity a where
    -- default entity
    empty :: a
    randomise :: a

    -- fitness
    getFitness :: (Ord b) => a -> b
    setFitness :: (Ord b) => a -> b -> a
    prepareForScoring :: a -> a
    score :: a -> a

    -- operators
    crossOver :: (a, a) -> Double -> (a, a)
    mutate :: a -> Double -> Double -> a
    
    -- various
    distance :: a -> a -> Double

