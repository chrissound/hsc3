module Sound.SC3.UGen.Composite where

import Sound.SC3.UGen.Demand.Monadic
import Sound.SC3.UGen.Filter
import Sound.SC3.UGen.Information
import Sound.SC3.UGen.IO
import Sound.SC3.UGen.Oscillator
import Sound.SC3.UGen.Noise.Monadic
import Sound.SC3.UGen.Panner
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UId

-- | Demand rate (:) function.
dcons :: (UId m) => UGen -> UGen -> m UGen
dcons x xs = do i <- dseq 1 (mce2 0 1)
                a <- dseq 1 (mce2 x xs)
                dswitch i a

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

-- | Collapse multiple channel expansion by summing.
mix :: UGen -> UGen
mix u | isMCE u = sum (mceProxies u)
      | otherwise = u

-- | Construct and sum a set of UGens.
mixFill :: Int -> (Int -> UGen) -> UGen
mixFill n f = mix (mce (map f [0..n-1]))

-- | Monadic variant on mixFill.
mixFillM :: (Monad m) => Int -> (Int -> m UGen) -> m UGen
mixFillM n f = mapM f [0 .. n - 1] >>= return . sum

-- | PM oscillator.
pmOsc :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen
pmOsc r cf mf pm mp = sinOsc r cf (sinOsc r mf mp * pm)

-- | Zero indexed audio input buses.
soundIn :: UGen -> UGen
soundIn n = in' 1 AR (numOutputBuses + n)

-- | Pan a set of channels across the stereo field.
splay :: UGen -> UGen -> UGen -> UGen -> UGen
splay i s l c = mix (pan2 i (mce p * s + c) 1) * l * (sqrt (1 / n))
    where n = fromIntegral (mceDegree i)
          m = n - 1
          p = map ( (+ (-1.0)) . (* (2 / m)) ) [0 .. m]

tChoose :: (UId m) => UGen -> UGen -> m UGen
tChoose t a = do r <- tiRand 0 (constant (length (mceChannels a))) t
                 return (select r a)

twChoose :: (UId m) => UGen -> UGen -> UGen -> UGen -> m UGen
twChoose t a w n = do i <- twindex t n w
                      return (select i a)
