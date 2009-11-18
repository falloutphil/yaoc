
module Data
  (
    MonteCarloUserData(..),
    PutCall(..)
  ) where


-- Holds contract and market data
-- specific to each individual instrument
-- and specified by the user.
data MonteCarloUserData = MonteCarloUserData
  { stock  :: Double,
    strike :: Double,
    putCall :: PutCall,
    volatility :: Double,
    expiry :: Double,
    interestRate :: Double,
    timeSteps :: Int,
    numOfSims :: Int }

-- Type to specify option Put/Call
data PutCall = Put | Call
 

