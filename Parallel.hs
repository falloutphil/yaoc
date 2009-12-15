module Parallel
  (
    foldChunks,
    parSum
  ) where


import Control.Parallel.Strategies (rwhnf,using,parList)

parSum :: [Double] -> Double
parSum xs = sum $ xs `using` parList rwhnf 

-- Spark a fold of each chunk and
-- combine the results. Only works
-- for associative folds.
-- We can't parallelise a fold directly
-- so we can either do a group of folds like
-- below, or we can force evaluation on a list
-- in parallel and then fold the result.  However
-- the second option is no good if the resulting
-- list is large, as every element is created before
-- the fold is started = big thunk!
foldChunks :: (a -> b -> a) -> a -> [[b]] -> [a]
foldChunks foldFunc acc = map $ foldl foldFunc acc

