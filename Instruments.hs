{-# LANGUAGE ExistentialQuantification, BangPatterns #-}

module Instruments
  (
    existentialEvolve,
    existentialPayOff,
    European(..),
    Lookback(..),
    Binary(..),
    Asian(..)
  ) where

import Data
import Control.Parallel.Strategies

type EvolveFn = MonteCarloUserData -> Double -> Double -> Double
type PayOffFn = PutCall -> Double -> Double -> Double


existentialEvolve :: MonteCarloUserData -> ExistentialInstrument -> Double -> ExistentialInstrument
existentialEvolve ud (ExistentialInstrument !a) norm =
    ExistentialInstrument $ evolve ud a norm

existentialPayOff :: MonteCarloUserData -> ExistentialInstrument -> Double
existentialPayOff ud (ExistentialInstrument !a) = payOff ud a


-- Used for path independent options
evolveClosedForm :: EvolveFn
evolveClosedForm ud currStock normal = 
  let delta_t    = (expiry ud) / (fromIntegral (timeSteps ud))
      vol        = volatility ud
      drift      = ((interestRate ud) - (0.5*vol*vol)) * delta_t
      stochastic = vol * normal * sqrt delta_t
      multiplier = exp $ drift + stochastic
    in currStock * multiplier

-- Base function for path dependant options
evolveStandardForm :: EvolveFn
evolveStandardForm ud currStock normal =
   let delta_t    = (expiry ud) / (fromIntegral (timeSteps ud))
       vol        = volatility ud
       drift      = delta_t * interestRate ud
       stochastic = vol * normal * sqrt delta_t
       multiplier = 1 + drift + stochastic
     in currStock * multiplier 

-- Helper function for Put/Call arithmetic 
putCallMult :: Num a => PutCall -> a
putCallMult Call = 1
putCallMult Put = -1

-- Base function for pay offs
payOffStandard :: PayOffFn
payOffStandard putcall strikeVal stock =
  max 0 $ (putCallMult putcall)*(stock - strikeVal) 


-- Now for the concrete options using the above
-- abstract framework.

-- ******** EUROPEAN OPTION ********

newtype European = European Double

instance Instrument European where
    evolve ud (European stock) normal =
        European $ evolveClosedForm ud stock normal
    payOff ud (European stock) =
        payOffStandard (putCall ud) (strike ud) stock

-- ******** BINARY OPTION ********

newtype Binary = Binary Double

instance Instrument Binary where
    evolve ud (Binary stock) normal =
        Binary $ evolveClosedForm ud stock normal
    payOff ud (Binary stock) =
        if payOffStandard (putCall ud) (strike ud) stock > 0 then 1 else 0

-- ******** LOOKBACK OPTION ********

newtype Lookback = Lookback (Double,Double)

instance Instrument Lookback where
    evolve ud (Lookback (maxV,stock)) normal =
        let newStock = evolveStandardForm ud stock normal
           in Lookback $ (max maxV newStock, newStock)
    payOff ud (Lookback (maxV,_)) =
        payOffStandard (putCall ud) (strike ud) maxV


-- ******** ASIAN OPTION ********

newtype Asian = Asian (Double,Double)

instance Instrument Asian where
    evolve ud (Asian (total,stock)) normal =
        let newStock = evolveStandardForm ud stock normal
           in Asian $ (total+newStock, newStock)
    payOff ud (Asian (total,_)) =
        payOffStandard (putCall ud) (strike ud) (total / (fromIntegral $ timeSteps ud))