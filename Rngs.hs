
module Rngs
  (
    randomChunks,
    haltonNorm,
    ranq1Norm
  ) where

import GHC.Conc (numCapabilities)
import Data.List.Split (chunk)
import Data.Word (Word64)
import Data.Bits (shift,xor)

import Normal
import Data


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
randomChunks :: (Int->Int->[[Double]]) -> MonteCarloUserData -> Int -> [[[Double]]] 
randomChunks rndGen ud initialState = 
  let sims     = numOfSims ud
      ts       = timeSteps ud
      split    = sims `div` numCapabilities
      leftover = sims `mod` numCapabilities
      splits   = take numCapabilities $ iterate (+split) (initialState+leftover)
      divRnds  = [ take split $ rndGen nextState ts | nextState <- splits ]
    in if leftover == 0 then divRnds 
       else [ take leftover $ rndGen initialState ts ] ++ divRnds



-- ***** HALTON QRNG *****

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

-- Infinite list of primes
primes :: [Int]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]
        sieve _      = error "sieve called with empty list"

-- You tried a list comprehension here
-- it was very pretty, but twice as slow!   
reflect :: Double -> Double -> (Int,Int) -> Double
reflect _ h (0,_) = h
reflect f h (k,base) = reflect newF newH (newK,base)
  where
    newK = k `div` base
    newF = f / fromIntegral base
    newH = h + fromIntegral(k `mod` base) * newF


-- ***** RANQ1 PRNG *****

ranq1Norm :: Int -> Int -> [[Double]]
ranq1Norm seed totalDims =
  let normalise = invnorm . convertToDouble
    in [ map normalise rnds | 
         rnds <- chunk totalDims $ iterate ranq1Increment $ (initialize.fromIntegral) seed ]

initialize :: Word64 -> Word64
initialize = convertToWord64 . 
             ranq1Increment  . 
             ( xor 4101842887655102017 )

ranq1Increment :: Word64 -> Word64
ranq1Increment =  ( `ranq1XorShift` (-4) ) . 
                  ( `ranq1XorShift` 35 )   . 
                  ( `ranq1XorShift` (-21) ) 

convertToDouble :: Word64 -> Double
convertToDouble = (*5.42101086242752217E-20) . 
                  fromIntegral               . 
                  convertToWord64

convertToWord64 :: Word64 -> Word64
convertToWord64 = (*2685821657736338717)
  
ranq1XorShift :: Word64 -> Int -> Word64
ranq1XorShift v = (xor v) . (shift v)



 
