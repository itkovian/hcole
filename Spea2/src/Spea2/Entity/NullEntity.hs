-----------------------------------------------------------------------------
--
-- Module      :  Spea2.Entity.NullEntity
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
module Spea2.Entity.NullEntity
    ( NullEntity
    ) where

import Spea2.Entity

data NullEntity {
  fitness :: Double
  isScores :: Bool
}

instance Spea2.Entity NullEntity where
    empty :: NullEntity
    empty = NullEntity { fitness = 0.0
                       , isScored = False
                       }
    randomise = empty

    getFitness e = fitness e
    setFitness e f = e { fitness = f }
    prepareForScoring = id
    score = id

    crossOver et _ = et
    mutate = id

    distance _ _ = 0.0

