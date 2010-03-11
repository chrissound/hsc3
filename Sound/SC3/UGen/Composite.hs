-- | Common unit generator graphs.
module Sound.SC3.UGen.Composite where

import Control.Monad
import Data.List
import Data.List.Split
import Sound.SC3.UGen.Buffer
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Information
import Sound.SC3.UGen.IO
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Oscillator
import Sound.SC3.UGen.Panner
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen

-- | Dynamic klank, set of non-fixed resonating filters.
dynKlank :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
dynKlank i fs fo ds s = gen (mceChannels s)
    where gen (f:a:d:xs) = ringz i (f * fs + fo) (d * ds) * a + gen xs
          gen _ = 0

-- | Frequency shifter, in terms of Hilbert UGen.
freqShift :: UGen -> UGen -> UGen -> UGen
freqShift i f p = mix (h * o)
    where o = sinOsc AR f (mce [p + 0.5 * pi, p])
          h = hilbert i

-- | Linear interpolating variant on index.
indexL :: UGen -> UGen -> UGen
indexL b i =
    let x = index b i
        y = index b (i + 1)
    in linLin (frac i) 0 1 x y

-- | Collapse possible mce by summing.
mix :: UGen -> UGen
mix = sum . mceChannels

-- | Mix variant, sum to n channels.
mixN :: Int -> UGen -> UGen
mixN n u =
    let xs = transpose (splitEvery n (mceChannels u))
    in mce (map sum xs)

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (mce (map f [0..n-1]))

-- | Monadic variant on mixFill.
mixFillM :: (Monad m) => Int -> (Int -> m UGen) -> m UGen
mixFillM n f = liftM sum (mapM f [0 .. n - 1])

-- | PM oscillator.
pmOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

-- | Zero indexed audio input buses.
soundIn :: UGen -> UGen
soundIn (MCE ns) | all (==1) $ zipWith (-) (tail ns) ns =
    in' (length ns) AR (numOutputBuses + head ns)
soundIn n =
    in' 1 AR (numOutputBuses + n)

-- | Pan a set of channels across the stereo field.
splay :: UGen -> UGen -> UGen -> UGen -> UGen
splay i s l c = mix (pan2 i (mce p * s + c) 1) * l * sqrt (1 / n)
    where n = fromIntegral (mceDegree i)
          m = n - 1
          p = map ( (+ (-1.0)) . (* (2 / m)) ) [0 .. m]
