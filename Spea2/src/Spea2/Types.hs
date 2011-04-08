-----------------------------------------------------------------------------
--
-- Module      :  Spea2.Types
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

module Spea2.Types
    ( Spea2Configuration
    , Spea2Population
    , Spea2Generation
    , Entity
    ) where

import Spea2.Entity

--type Population = (Entity a) => [a]
--type Generation = (Entity a) => [[a]]

-- | Spea2Configuration
data Spea2Configuration = Spea2Configuration
   { spea2PopulationCount :: Int         -- Number of populations
   , spea2PopulationSize :: Int          -- Size of each population (fixed)
   , spea2ArchiveSize :: Int             -- Size of the archive (best entities)
   , spea2MaximumGenerationCount :: Int  -- Maximum number of generation to tryJust
   , spea2CrossOverRate :: Double        -- Crossover rate between entities
   , spea2MutationRate :: Double         -- Mutation rate for each entity
   , spea2MigrationRate :: Double        -- Rate of migrations between two populations
   , spea2StopFitnessThreshold :: Double -- Stop criterium based on fitness
   }

-- | Spea2Generation
data Entity a => Spea2Generation a = Spea2Generation 
  { spea2Gen :: [a]
  , spea2GenNumber :: Int
  }

-- | Spea2Population
data Entity a => Spea2Population a = Spea2Population
  { spea2Pop :: [a]
  , spea2PopSize :: Int
  }

