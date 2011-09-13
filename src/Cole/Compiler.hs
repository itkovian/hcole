{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{- 
 - Interface to the compiler backend. Each actual compiler data structure should 
 - be an instance of the ColeCompiler class.
 -}


 module Cole.Compiler
    ( ColeCompilerType(..)
    ) where

import System.Console.CmdArgs

import           Cole.Benchmark
import           Cole.Entity
import           Cole.Compiler.Optimisation

--------------------------------------------------------------------------------
-- | Data type for the available compilers. 
--
-- FIXME: Not all of these have been implemented yet.
data ColeCompilerType = ColeGCC 
                      | ColeLLVM 
                      | ColeJikesRVM 
                      | ColeGHC 
                      | None
                      deriving (Show, Read, Data, Typeable)



---------------------------------------------------------------------------------
-- Class definition for the COLE compilers.
class Entity m e => ColeCompiler m a e where 
    -- |Compile the sucker. In some way.
    -- FIXME: This should probably be used on the backend in any case.
    compile :: a -> e -> IO ()
  
