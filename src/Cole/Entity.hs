{-# LANGUAGE MultiParamTypeClasses #-}

{-
 - (C) 2011, Andy Georges
 -
 - This module defines the Entity class for COLE experiments.
 -
 - The API is volatile at this point.
 -}

 module Cole.Entity 
    ( Entity(..)
    ) where

import           Cole.Model

-----------------------------------------------------------------------
-- |Type class for the Entity used in a COLE experiment.
--
-- The class is parametrised over a monad m and the actual entity 
-- datatype a.
--
-- It requires data types to implement at least the following 
-- functionality:
--
-- * genRandom : generates a new random entity from whatever source is
--               available. Thus it executes in the underlying monad (stack).
--
-- * executeScore : evaluates the entity and returns a score. It obviously executes
--                  in the underlying monad (stack). We keep the score a single (1D)
--                  metric.
--
-- * modelScore : evaluates the entity according to a model and returns its
--                score. We keep the score to a single (1D) metric.
--
class (Monad m) => Entity m a where
    -- |generates a random new entity specimen from a given seed.
    genRandom :: Int -> m a

    -- |evaluate an entity and return its score in the monad.
    executeScore :: a -> m Double

    -- |return the model's score for the given entity in the monad. 
    modelScore :: a -> Model -> m Double
