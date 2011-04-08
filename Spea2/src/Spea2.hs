-----------------------------------------------------------------------------
--
-- Module      :  Spea2
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

module Spea2
    ( evolutionFromArchive
    ) where


import Spea2.Types



performMigration = undefined
performCrossover = undefined
performMutation  = undefined

evolutionFromArchive :: Entity a => [Spea2Population a] -> [a]
evolutionFromArchive = undefined

{- Evolution pseudocode
 -
 - seed initial entities
 - prepare them so they can be scored (basically have them do the work and have them collect their data)
 - while population count < max population count {
 -    check that all entities in this population are ready to be scored (i.e., they all finished their tasks)
 -    score them by determining their fitness
 -    sort them according to fitness
 -    fill the initial archive with all non-dominated entities
 - }
 - evolve the entities to build the next generation while the generation count < max generation count:
 -    perform migration between populations
 -    perform crossover between the archive and the current generation
 -    perform mutation
 -    prepare all entities in the new generation for scoring
 -    wait until all entities are ready to be scored (and thus have completed whatever they do)
 -    determine their scores by means of the fitness function
 -    check if there are any improvements to be made to the Pareto front
 -    rescore the archive entities (they had already performed the work)
 -    foreach population:
 -        drop the current population from this generation and the archive for this population into a single pool
 -        sort this pool by fitness
 -        copy (unique) best entities to the archive for this population
 -        rebuild archive by checking which entities are the best and cover the front best (via the sandbox) by
 -                throwing out the entities that are closest to others (iteratievely) until there are as many
 -                non-dominated entities as there is size in the archive. copy these to the archive
 -    check for convergence (if no improvement for N generations, we call it quits
 -    check if we reached the maximum number of generations
 - determine the best entities for each population (based on the archive/all sets of the archives see thus far)
 - from these best entities, determine the set of non-dominated entities
 -
 -}
