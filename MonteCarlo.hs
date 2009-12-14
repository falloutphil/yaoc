
import Parallel
import Data
import Instruments
import Rngs


mc :: MonteCarloUserData -> [[[Double]]] -> Double
mc userData rndss = 
  (psum $ foldChunks f 0 rndss) / (fromIntegral $ numOfSims userData)
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
  
  let rngss = randomChunks userData 1
    in print $ discount userData $ mc userData rngss


