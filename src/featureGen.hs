{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{- 
 - This program transforms a given set of optimisation flags to a feature
 - vector that can serve as input for various machine learning algorithms
 -}

import qualified Data.ByteString as B
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import System.Console.CmdArgs
import System.Environment

type Pass = T.Text

instance Default T.Text where
  def = T.empty


data Options = Options { presence :: Bool
                       , minimalDist :: Bool
                       , maximalDist :: Bool
                       , passes :: String
                       , file :: Maybe FilePath
                       } deriving (Show, Data, Typeable)


options = Options { presence = def &= {- opt True &= -} typ "Bool" &= help "Counts the number of times a pass occurs in the sequence" 
                  , minimalDist = def &= {- opt True &= -} help "The minimal distances between pairs of passes"
                  , maximalDist = def &= {- opt False &= -} help "The maximal distances between pairs of passes"
                  , passes = def &= help "The allowed passes we check for"
                  , file = def &= typFile &= help "Should we get the pass sequence from this file?"
                  } 
          &= verbosity
          &= program "featureGen"
          &= summary "featureGen v 0.0.1"
          &= help "Feature Generation for modeling optimisation pass sequences."


-- | Get the unique elements from a list
uniq :: Ord a => [a] -> [a]
uniq = map head . group . sort

-- | Derive the features for presence of a pass
presencePass :: [Pass]        -- Optimisation pass sequence
             -> [(Pass, Int)] -- Resulting features. A tuple is the (pass, #appearances)
presencePass ps = map (\pss@(p:ps) -> (p, length pss)) $ group . sort $ ps


-- | Minimal distance between two optimisation passes
--
minimalDistances :: [Pass]                 -- The optimisation pass sequence
                 -> [((Pass, Pass), Int)]  -- Resulting distances. We return values for all combinations.
minimalDistances ps = distances ps (head . sort) 

-- | Maximal distance between two optimisation passes
maximalDistances :: [Pass]                 -- The optimisation pass sequence
                 -> [((Pass, Pass), Int)]  -- Resulting distances. We return values for all combinations.
maximalDistances ps = distances ps (last . sort) 


-- | Parametrised distance function
--
-- We only consider passes in the order they appear, i.e., for a . . . . b, we have a value for (a,b)
-- but not for (b,a). The reason is that we might have an a . . b . . . a, which then can yield 4 for 
-- (b, a) instead of -3. 
--
-- We return a list of all pairs for which the distance is strictly positive
distances :: [Pass]                 -- The optimisation pass sequence
          -> ([Int] -> Int)            -- Distance selection mechanism
          -> [((Pass, Pass), Int)] -- Resulting distances for a pair of features.
distances ps f = 
    let fs = uniq ps
        pairs = [(a, b) | a <- fs, b <- fs]
    in catMaybes $ map mds pairs
  where is = zip [1..] ps
        mds :: (Pass, Pass) -> Maybe ((Pass, Pass), Int)
        mds (a,b) = 
            let as = filter ((==) a . snd) is 
                bs = filter ((==) b . snd) is
                ds = [ j - i | (i, _) <- as  -- indices of the a-pass
                                   , (j, _) <- bs  -- indices of the b-pass 
                                   , i /= j        -- if a == b then we must not consider the same index, otherwise the distance will be 0
                                   , i < j         -- strictly speaking this includes the previous demand; we want a to appear before b      
                                   ]
            in if null ds 
                 then Nothing 
                 else Just ((a, b), f ds)

fixedFormatPresence :: [Pass]        -- All passes that can be used in the canonical order
                    -> [(Pass, Int)] -- Presence list
                    -> [Int]         -- Presence values for all passes in the canonical order
fixedFormatPresence ps presences =
    let mp = M.fromList presences
    in map (\p -> case M.lookup p mp of
                    Nothing -> 0
                    Just v  -> v) ps 

fixedFormatDistance :: [Pass]                 -- All passes that can be used in the canonical order 
                    -> [((Pass, Pass), Int)]  -- Non-zero distances between passes
                    -> [Int]                  -- Distance values for all pass pairs on the canonical order 
fixedFormatDistance ps distances =
    let md = M.fromList distances
    in map (\pp -> case M.lookup pp md of
                     Nothing -> 0
                     Just d  -> d) $ [(a,b) | a <- ps, b <- ps]


process :: Options -- command line arguments to apply to the feature generation
        -> T.Text  -- line with the pass sequence
        -> T.Text  -- string with the resulting features
process args line = 
    let ps = T.splitOn "-" line
        presences = presencePass ps
        mins = minimalDistances ps
        maxs = maximalDistances ps
        pss = T.splitOn "," $ T.pack $ passes args
    in T.intercalate "," $ catMaybes
                           [ case presence args of 
                                True -> Just $ t $ fixedFormatPresence pss presences
                                False -> Nothing
                           , case minimalDist args of
                                True -> Just $ t $ fixedFormatDistance pss mins 
                                False -> Nothing
                           , case maximalDist args of
                                True -> Just $ t $ fixedFormatDistance pss maxs
                                False -> Nothing
                           ]
  where t = T.intercalate "," . map (T.pack . show) 


  


main = do 
  args <- cmdArgs options

  lss <- (T.lines . E.decodeUtf8) `fmap` case file args of
                                            Nothing -> B.getContents
                                            Just f  -> B.readFile f

  mapM (B.putStrLn . E.encodeUtf8) $ map (process args) lss

