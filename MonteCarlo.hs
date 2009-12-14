
import Data.List

import Parallel
import Data
import Instruments
import Rngs

import Control.Parallel.Strategies 

-- chunking on the rndss is forcing valuation on them???
-- the list of rngs remains an 'idea' rather than an implementation
-- when we don't iterate over it.... perhaps?
-- YES - don't manipulate the rndss list, create two from scratch! BINGO!
mc :: MonteCarloUserData -> [[[Double]]] -> Double
mc userData rndss = 
  ((psum rwhnf) $ foldChunks f 0 rndss) / (fromIntegral $ numOfSims userData)
  --(foldl f 0 rndss) / (fromIntegral $ numOfSims userData)
    where f           = flip $ (+) . payOff' . expiryValue 
          payOff'     = existentialPayOff userData
          expiryValue = foldl (existentialEvolve userData) (stock userData)
    
discount userData = (*) (exp ( (-(interestRate userData)) * 
                             (expiry userData) ))  

main :: IO()
main = do
  let userData = MonteCarloUserData { stock        = ExistentialInstrument $ European 100,
                                      strike       = 100,
                                      putCall      = Call,
                                      volatility   = 0.2,
                                      expiry       = 1,
                                      interestRate = 0.05,
                                      timeSteps    = 1,
                                      numOfSims    = 10000000 }
  
  let rngss = haltonNorm (1,timeSteps userData)
    in print $ discount userData $ mc userData rngss


