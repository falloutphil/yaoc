
import Data.List

import Data
import Instruments
import Rngs

import Control.Parallel.Strategies

-- f has to be of the form (McInstrument->Double->McInstrument)
-- to use in fold.
-- But clearly (Double->McInstrument->McInstrument) gives nicer
-- point free semantics. as h x y = g . f where f(a) and g(b,c)
-- dictates that x->a and f(x)->b and y->c.
-- First param goes to inner most call - for this we NEED this
-- to be the list of normal variates -> expiryValue
-- So here we construct add to be good for point free semantics
-- but flip it so the final function is add with it's params
-- swapped to match the signature of fold.
-- The other option of course is to make add fold-friendly
-- and the use a lambda expression to tie the inner most
-- parameter to the lhs of add.
-- f = \accumulator = (add accumulator) . payOff' expiryValue
-- Obviously you'll have to swap the defn of add too!


pfoldl :: (a -> b -> a) -> a -> [b] -> a
pfoldl f acc xs = foldl f acc (xs `using` parList rwhnf)

-- Note now abstract types are used and f is of type
-- ExistentialInstr -> Double -> ExistentialInstr
-- The above still applies, we are just initialising
-- the fold from our CalculationParams templates (existenInst) and
-- have to conduits existenitalAdd and existentialResult
-- that pry open our ExistentialInstr and call the underlying
-- add and result functions in the underlying typeclass.
-- This allows us to hide the polymorphism well away from
-- the main program.
mc :: MonteCarloUserData -> [[Double]] -> Double
mc userData rndss = 
  existentialResult (pfoldl f existenAvg rndss) $ numOfSims userData
    where f           = flip $ existentialCombine . payOff' . expiryValue 
          payOff'     = existentialPayOff userData
          expiryValue = foldl' (existentialEvolve userData) (stock userData)
          existenAvg  = averager userData
    
        
discount userData = (*) (exp ( (-(interestRate userData)) * 
                             (expiry userData) ))  

main :: IO()
main = do
  let userData = MonteCarloUserData { stock        = ExistentialInstrument $ European 100,
                                      averager     = ExistentialAverage $ Arithmetic 0,
                                      strike       = 100,
                                      putCall      = Call,
                                      volatility   = 0.2,
                                      expiry       = 1,
                                      interestRate = 0.05,
                                      timeSteps    = 1,
                                      numOfSims    = 100000 }
  
  let rngss = take (numOfSims userData) $ haltonNorm (1,timeSteps userData)
    in print $ discount userData $ mc userData rngss


