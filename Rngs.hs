
module Rngs
  (
    haltonNorm
  ) where

import Normal

primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
        sieve _      = error "sieve called with empty list"

-- Primes represent our dimensions
-- that is our sample points in time
-- Our seed is incremented for each run.
-- Thus hold the seed constant across our
-- number of dims and iterate our prime list
-- to create a pair to map onto.
haltonNorm :: (Int,Int) -> [[[Double]]]
haltonNorm (initialState,totalDims) =
  let normalise = invnorm . (reflect 1 0)
    in
      [ [ map normalise $ zip (replicate totalDims seed) primes
          | seed <- [initialState..5000000]  ], 
        [ map normalise $ zip (replicate totalDims seed) primes
          | seed <- [initialState+5000000..10000000]  ] 
      ]
   
reflect :: Double -> Double -> (Int,Int) -> Double
reflect f h (0,base) = h
reflect f h (k,base) = reflect newF newH (newK,base)
  where
    newK = k `div` base
    newF = f / fromIntegral base
    newH = h + fromIntegral(k `mod` base) * newF

