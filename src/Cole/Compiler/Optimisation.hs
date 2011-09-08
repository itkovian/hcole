{- 
 - Definition of the optimisation class. 
 - All actual optimisation data structures need to be instances of this class.
 -}

module Cole.Compiler.Optimisation
    (
    ) where

import qualified Data.Text as T

---------------------------------------------------------------------------------
--
class Eq a => ColeCompilerOptimisation a where
    toArgument :: a -> T.Text
