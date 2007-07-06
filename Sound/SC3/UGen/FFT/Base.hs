module Sound.SC3.UGen.FFT.Base where

import Sound.SC3.UGen.Rate (Rate(KR))
import Sound.SC3.UGen.UGen (UGen, UGenId)
import Sound.SC3.UGen.UGen.Construct (mkOscId)

-- | Randomize order of bins.
pv_BinScramble :: UGenId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_BinScramble z buf wp width trg = mkOscId z KR "PV_BinScramble" [buf,wp,width,trg] 1

-- | Randomly clear bins.
pv_RandComb :: UGenId -> UGen -> UGen -> UGen -> UGen
pv_RandComb z buf wp trg = mkOscId z KR "PV_RandComb" [buf,wp,trg] 1

-- | Cross fade, copying bins in random order.
pv_RandWipe :: UGenId -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RandWipe z ba bb wp trg = mkOscId z KR "PV_RandWipe" [ba,bb,wp,trg] 1
