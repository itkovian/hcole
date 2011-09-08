{-
 - Interface to the benchmarks. Each actual benchmark data structure should
 - be an instance of the ColeBenchmark class.
 -}

 module Cole.Benchmark
    ( ColeSuiteType
    ) where

--------------------------------------------------------------------------------
-- | Data type for the available benchmark suites. 
--
-- FIXME: Not all of these have been implemented
data ColeSuiteType = ColeSPECCPU2000 
                   | ColeSPECCPU2006 
                   | ColeDacapo 
                   | ColeSPECJVM98
                   deriving (Show, Read, Typeable)



---------------------------------------------------------------------------------
--
class ColeBenchmark a where
    run :: a -> IO ()
