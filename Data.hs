{-# LANGUAGE ExistentialQuantification #-}

module Data
  (
    MonteCarloUserData(..),
    PutCall(..),
    ExistentialInstrument(..),
    Instrument(..)

  ) where


class Instrument a where
    evolve :: MonteCarloUserData -> a -> Double -> a
    payOff :: MonteCarloUserData -> a -> Double

data ExistentialInstrument = forall i. (Instrument i) => ExistentialInstrument i

-- Holds contract and market data
-- specific to each individual instrument
-- and specified by the user.
data MonteCarloUserData = MonteCarloUserData
  { stock  :: ExistentialInstrument,
    strike :: Double,
    putCall :: PutCall,
    volatility :: Double,
    expiry :: Double,
    interestRate :: Double,
    timeSteps :: Int,
    numOfSims :: Int }

-- Type to specify option Put/Call
data PutCall = Put | Call
 

