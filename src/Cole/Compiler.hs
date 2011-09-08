{- 
 - Interface to the compiler backend. Each actual compiler data structure should 
 - be an instance of the ColeCompiler class.
 -}


 module Cole.Compiler
    ( ColeCompilerType
    ) where

import           Cole.Benchmark
import           Cole.Compiler.Optimisation

--------------------------------------------------------------------------------
-- | Data type for the available compilers. 
--
-- FIXME: Not all of these have been implemented yet.
data ColeCompilerType = ColeGCC 
                      | ColeLLVM 
                      | ColeJikesRVM 
                      | ColeGHC 
                      deriving (Show, Read, Typeable)



---------------------------------------------------------------------------------
-- Class definition for the COLE compilers.
class ColeCompiler a where 
  compile :: a -> OptimisationSequence -> IO ()
  
