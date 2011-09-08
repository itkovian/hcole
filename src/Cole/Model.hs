{-# LANGUAGE MultiParamTypeClasses #-}

{-
 - (C) 2011, Andy Georges
 -
 - This module defines the Model class for COLE experiments.
 -
 -}

 module Cole.Model
    ( Model(..)
    ) where

--------------------------------------------------------------------------------
-- | Data type for the available modeling backends
--
-- FIXME: not all of these have been implemented yet.
data ColeModelType = ColeM5
                   | ColeRepTree
                   | ColeSVM

-----------------------------------------------------------------------
-- |Type class for the Model used in a COLE experiment. 
--
-- Each data type that is a Model instance should at least implement
-- the following functions
--
-- * train : from a given set of training examples, build a model that
--           approaches the target values in the training set as closely
--           as possible under some error rule, e.g., least mean squares.
--
-- * predict :: given source data, predict what will be the resulting 
--              score under the model.
--
-- Because a model might need to do some IO or interact with the world
-- at large, keep track of information etc, we base the model on a monad
-- stack.
--
class (Monad m) => Model m a b where
    -- | train on the given set of data
    train :: [(a,b)] -> Model m a b

    -- | predict using a new piece of information
    predict :: Model m a b -> a -> m b


