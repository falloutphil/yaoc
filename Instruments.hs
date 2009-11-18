{-# LANGUAGE ExistentialQuantification, BangPatterns #-}

module Instruments
  (
    CalculationParams(averager, evolver, payOff),
    existentialCombine,
    existentialResult,
    europeanParms    
  ) where

import Data

type EvolveFn = MonteCarloUserData -> Double -> Double -> Double

type PayOffFn = PutCall -> Double -> Double -> Double

class McAverager a where
    combine    :: Double -> a -> a
    result :: a -> Double

-- For every McInstrument create a c'tor for ExistentialInstr
data ExistentialAverage = forall i. (McAverager i) => ExistentialAverage i

-- Given an ExistentialInstr and a new single sim run
-- add the result to the underlying instrument and
-- wrap it back up in to a ExistentialInstr.
-- This works because b is of type McInstrument and
-- will have a concrete add method defined.
-- Note we CANNOT get at b itself because within the
-- scope of this function we cannot know what b is.
-- But we can call functions in its typeclass which
-- will then know how to deal with it.
existentialCombine :: Double -> ExistentialAverage -> ExistentialAverage
existentialCombine a (ExistentialAverage !b) = 
  ExistentialAverage $ combine a b 

existentialResult :: ExistentialAverage -> Double
existentialResult (ExistentialAverage a) = result a
 
-- Must use Existential Qualification to hide true
-- instrument type inside a wrapper, so that Haskell
-- can treat it as one single type.
data CalculationParams = CalculationParams
  { averager :: ExistentialAverage,
    evolver  :: EvolveFn,
    payOff   :: PayOffFn }

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

newtype Arithmetic = Arithmetic Double

instance McAverager Arithmetic where
    combine a (Arithmetic b)  = Arithmetic $ a + b
    result (Arithmetic a)  = a 

-- Just to make it obvious!
europeanEvolver = evolveClosedForm
europeanPayOff  = payOffStandard


-- Concrete tuple of calculation details
-- for European
europeanParms  = CalculationParams
  { averager   = ExistentialAverage $ Arithmetic 0,
    evolver    = europeanEvolver,
    payOff     = europeanPayOff }



-- ******** LOOKBACK OPTION ********

{-
newtype Lookback = Lookback Double
 
instance McInstrument Lookback where
    add a  (Lookback b) = Lookback $ a + b
    result (Lookback a) = a 

lookbackEvolver ud (maxStock,currStock) normal =
  let evolveStandardForm ud currStock normal = newValue
    in if newValue > maxStock then (newValue,newValue) else (maxStock,newValue)

lookbackPayOff putcall strikeVal (maxStock,_) =
  payOffStrandard putcall strikeVal maxStock

payOffLookback 
-- Concrete tuple of calculation details
-- for European
lookbackParms  = CalculationParams
  { instrument = ExistentialInstr $ Lookback (0,0),
    evolver    = lookbackEvolver,
    payOff     = lookbackPayOff }
-}