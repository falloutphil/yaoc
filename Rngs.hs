
module Rngs
  (
    randomChunks
  ) where

import GHC.Conc (numCapabilities)

import Normal
import Data

-- Infinite list of primes
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
-- This creates a list of normals for each
-- run, and thus a list of lists in total.
haltonNorm :: Int -> Int -> [[Double]]
haltonNorm initialState totalDims =
  let normalise = invnorm . (reflect 1 0)
    in [ map normalise $ zip (replicate totalDims seed) primes
         | seed <- [initialState..]  ]
   
reflect :: Double -> Double -> (Int,Int) -> Double
reflect f h (0,base) = h
reflect f h (k,base) = reflect newF newH (newK,base)
  where
    newK = k `div` base
    newF = f / fromIntegral base
    newH = h + fromIntegral(k `mod` base) * newF

-- We need to chunkify haltonNorm into a parent
-- list, where each element in the parent list is a list of lists produced
-- by the above.  The idea is that each element is a package
-- of normals to be send to a particular core for simulation.
-- Thus we have a list of lists of lists.... christ.
-- It is important to produce this structure as part of the 
-- generating function.  If at a later date we tried to chunkify
-- the output from haltonNorm we would have to traverse each element
-- and thus each element would be brought into existance by the generating
-- function.  This produces on almighty thunk.  So we create the construct
-- here and then only pop items off each list as we need them.
randomChunks :: MonteCarloUserData -> Int -> [[[Double]]] 
randomChunks ud initialState = 
  let sims     = numOfSims ud
      ts       = timeSteps ud
      split    = sims `div` numCapabilities
      leftover = sims `mod` numCapabilities
      splits   = take numCapabilities $ iterate (+split) (initialState+leftover)
      divRnds  = [ take split $ haltonNorm nextState ts | nextState <- splits ]
    in if leftover == 0 then divRnds 
       else divRnds ++ [ take leftover $ haltonNorm initialState ts ]
 
