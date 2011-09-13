{- 
 - (C) 2011 Andy Georges
 -
 - Module that represents the LLVM compiler.
 -
 -}

 module Cole.Compiler.LLVM
    (
    
    ) where

import           System.Random (StdGen, mkStdGen, randoms)

import Cole.Entity
import Cole.Compiler

data MutationType = Tweak | Add | Delete

data LLVMEntity = LLVMEntity
    { maxSize :: Int
    , size :: Int
    , opts :: Int
    }

data LLVMCompiler = LLVMCompiler
    { path :: FilePath
    }

instance Show LLVMEntity where
    show (LLVMEntity l os) = concat . intersperse "-" . map show $ os


instance Entity LLVMEntity where
    -- |generation of a random entity
    genRandom seed = do
        let l:rs = randoms seed
            os = take (l `mod` XXX) $ map (`mod` YYY) rs  
        return $ LLVMEntity { length = l
                            , opts = os
                            }
           

    crossoverOnePoint _ seed e1 e2 = 
        let ml = maxSize e1
            l1 = size e1
            l2 = size e2
            (s1:s2:_) = randoms seed
            -- FIXME: we might need to drop it if it takes too long to generate a new size of acceptable magnitude 
            --        COLE uses 100 trials.
            (newSize, (i1, i2)) = head $ dropWhile ((>) ml . fst) $ [ (i1 `mod` l1 + (size e2) - i2 `mod` l2, (i1 `mod` l1, i2 `mod` l2)) | i1 <- randoms s1, i2 <- randoms s2]
            newPasses1 = take i1 (opts e1) `mappend` take i2 (opts e2)
            newPasses2 = take i2 (opts e2) `mappend` take i1 (opts e1)

            e1' = LLVMEntity { maxSize = ml
                             , size = i1 + i2
                             , opts = newPasses1
                             }
            e2' = LLVMEntity { maxSize = ml
                             , size = i1 + i2
                             , opts = newPasses2
                             }

            return $ (e1', e2')

    -- mutationMultiPointDrift :: (Float, Float) -> Int -> a -> Maybe a
    mutationMultiPointDrift (p1, p2) seed e = 
        let l1 = size e1
            t1 = round $ 1 / p1
            t2 = round $ 1 / p2
            (kind:rs) = dropWhile (\v -> l1 == 1 && v `mod` 3 == 2) $ randoms seed -- FIXME: this should use the mutationtype instead
        in case kind of
            -- Tweak
            0 -> let os = zipWith (\(r, o) -> if r `mod` t1 == 0 then newPass r l1 else o) rs opts
                 in Just $ LLVMEntity { maxSize = maxSize e
                               , size = l1
                               , opts = os
                               }
            -- Add
            1 -> let os = concat $ zipWith (\(r, o) -> if r `mod` t2 == 0 then [o,newPass r s]) rs opts
                 in Just $ LLVMEntity { maxSize = maxSize e
                               , size = length os
                               , opts = os
                               }
            -- Delete
            2 -> let os = concat $ zipWith (\(r, o) -> if r `mod` t2 == 0 then [] else [o]) rs opts
                 in Just $ LLVMEntity { maxSize = maxSize e
                               , size = length os
                               , opts = os
                               }
      where newPass r s = flip mod s . head $ randoms r



