{-# LANGUAGE ExistentialQuantification #-}

module Data
  (
    MonteCarloUserData(..),
    PutCall(..),
    Instrument(..)
  ) where


class Instrument a where
    evolve :: MonteCarloUserData -> a -> Double -> a
    payOff :: PutCall -> Double -> a -> Double

-- Holds contract and market data
-- specific to each individual instrument
-- and specified by the user.
data MonteCarloUserData = forall i. (Instrument i) => MonteCarloUserData
  { stock  :: i,
    strike :: Double,
    putCall :: PutCall,
    volatility :: Double,
    expiry :: Double,
    interestRate :: Double,
    timeSteps :: Int,
    numOfSims :: Int }

-- Type to specify option Put/Call
data PutCall = Put | Call
 

