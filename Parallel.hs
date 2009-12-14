module Parallel
  (
    foldChunks,
    psum
  ) where


import Control.Parallel.Strategies (rwhnf,using,parList)

psum :: [Double] -> Double
psum xs = sum (xs `using` parList rwhnf) 

-- Spark a fold of each chunk and
-- combine the results. Only works because
-- for associative folds.
foldChunks :: (a -> b -> a) -> a -> [[b]] -> [a]
foldChunks foldFunc acc = map $ foldl foldFunc acc

