
import Data.List

import Data
import Instruments
import Rngs

import Control.Parallel.Strategies (rwhnf,parMap)
import Data.List.Split (chunk)
import GHC.Conc (numCapabilities)

-- Prepare to share work to be 
-- done across available cores
chunkOnCpu :: [a] -> [[a]]
chunkOnCpu xs = chunk (length xs `div` numCapabilities) xs
 
-- Spark a fold of each chunk and
-- combine the results. Only works because
-- for associative folds.
foldChunks :: ([a] -> a) -> (a -> b -> a) -> a -> [[b]] -> a
foldChunks combineFunc foldFunc acc = 
  combineFunc . (parMap rwhnf $ foldl' foldFunc acc)


mc :: MonteCarloUserData -> [[Double]] -> Double
mc userData rndss = 
  (foldChunks sum f 0 $ chunkOnCpu rndss) / (fromIntegral $ numOfSims userData)
    where f           = flip $ (+) . payOff' . expiryValue 
          payOff'     = existentialPayOff userData
          expiryValue = foldl' (existentialEvolve userData) (stock userData)
    
discount userData = (*) (exp ( (-(interestRate userData)) * 
                             (expiry userData) ))  

main :: IO()
main = do
  let userData = MonteCarloUserData { stock        = ExistentialInstrument $ Lookback (0,100),
                                      strike       = 100,
                                      putCall      = Call,
                                      volatility   = 0.2,
                                      expiry       = 1,
                                      interestRate = 0.05,
                                      timeSteps    = 10,
                                      numOfSims    = 1000000 }
  
  let rngss = take (numOfSims userData) $ haltonNorm (1,timeSteps userData)
    in print $ discount userData $ mc userData rngss


