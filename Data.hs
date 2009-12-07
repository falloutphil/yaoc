{-# LANGUAGE ExistentialQuantification #-}

module Data
  (
    MonteCarloUserData(..),
    PutCall(..),
    ExistentialInstrument(..),
    Instrument(..),
    ExistentialAverage(..),
    McAverager(..)
  ) where


class Instrument a where
    evolve :: MonteCarloUserData -> a -> Double -> a
    payOff :: MonteCarloUserData -> a -> Double

data ExistentialInstrument = forall i. (Instrument i) => ExistentialInstrument i

class McAverager a where
    combine :: Double -> a -> a
    result  :: a -> Int ->  Double

-- For every McInstrument create a c'tor for ExistentialInstr
data ExistentialAverage = forall i. (McAverager i) => ExistentialAverage i

-- Holds contract and market data
-- specific to each individual instrument
-- and specified by the user.
data MonteCarloUserData = MonteCarloUserData
  { stock  :: ExistentialInstrument,
    averager :: ExistentialAverage,
    strike :: Double,
    putCall :: PutCall,
    volatility :: Double,
    expiry :: Double,
    interestRate :: Double,
    timeSteps :: Int,
    numOfSims :: Int }

-- Type to specify option Put/Call
data PutCall = Put | Call
 

