-- Gnuplot imports
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import Graphics.Gnuplot.Plot.TwoDimensional (linearScale, )

-- Local imports
import Parallel
import Data
import Instruments
import Rngs

-- Monte Carlo Stuff
mc :: MonteCarloUserData -> [[[Double]]] -> Double
mc userData rndsss = 
  (parSum $ foldChunks f 0 rndsss) / (fromIntegral $ numOfSims userData)
    where f           = flip $ (+) . payOff' . expiryValue 
          payOff'     = existentialPayOff userData
          expiryValue = foldl (existentialEvolve userData) (stock userData)
    
discount userData = (*) (exp ( (-(interestRate userData)) * 
                             (expiry userData) ))  

-- Option Wrappers
european expiry stock =
  let userData = MonteCarloUserData { stock        = ExistentialInstrument $ European stock,
                                      strike       = 100,
                                      putCall      = Call,
                                      volatility   = 0.2,
                                      expiry       = expiry,
                                      interestRate = 0.05,
                                      timeSteps    = 1,
                                      numOfSims    = 10000 }
                 in discount userData $ mc userData (randomChunks haltonNorm userData 20)
                    
asian expiry stock =
  let userData = MonteCarloUserData { stock        = ExistentialInstrument $ Asian (stock,stock),
                                      strike       = 100,
                                      putCall      = Call,
                                      volatility   = 0.2,
                                      expiry       = expiry,
                                      interestRate = 0.05,
                                      timeSteps    = 100,
                                      numOfSims    = 1000 }
                 in discount userData $ mc userData (randomChunks ranq1Norm userData 20)


-- Graph functions
defltOpts :: Graph.C graph => Opts.T graph
defltOpts =
   Opts.remove Opt.key $
   Opts.deflt

euro3d :: Frame.T (Graph3D.T Double Double Double)
euro3d =
   let meshStock  = linearScale 35 (50,150)
       meshExpiry = linearScale 35 (0,5)
   in  Frame.cons
          (Opts.xRange3d (0,5) $
           Opts.yRange3d (50,150) $
           Opts.title "Euro Option Stuck @ 100" $ 
           Opts.xLabel "Time to Expiry" $
           Opts.yLabel "Stock" $
           Opts.zLabel "     Value" $
           defltOpts)  $
       Plot3D.surface
          meshExpiry meshStock european
          

-- HINT: ghc -O2 -threaded --make MonteCarlo.hs
--            ./MonteCarlo +RTS -N2 
main :: IO()
main = do 
  Plot.plot X11.cons euro3d
  Plot.plot (PNG.cons "euro.png") euro3d
  print "end"