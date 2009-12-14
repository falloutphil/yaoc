module Parallel
  (
    chunkOnCpu,
    foldChunks,
    psum
  ) where


import Control.Parallel.Strategies 
--(rwhnf,r0,parMap,using,parList)
import Data.List.Split (chunk)
import GHC.Conc (numCapabilities)
import Data.List (foldl')


-- Prepare to share work to be 
-- done across available cores
chunkOnCpu :: [a] -> [[a]]
chunkOnCpu xs = chunk (length xs `div` numCapabilities) xs
--chunkOnCpu xs =  take (length xs `div` numCapabilities) xs : 
--                 drop (length xs `div` numCapabilities) xs : []

psum :: Strategy Double -> [Double] -> Double
psum strat xs = sum (xs `using` parList strat) 

-- Spark a fold of each chunk and
-- combine the results. Only works because
-- for associative folds.
foldChunks :: (a -> b -> a) -> a -> [[b]] -> [a]
foldChunks foldFunc acc = map $ foldl foldFunc acc

