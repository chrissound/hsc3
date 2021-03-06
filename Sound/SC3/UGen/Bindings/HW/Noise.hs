-- | Non-deterministic noise 'UGen's.
module Sound.SC3.UGen.Noise.ID where

import Sound.SC3.UGen.Identifier
import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.Type
import Sound.SC3.UGen.UGen

-- | Brown noise.
brownNoise :: ID a => a -> Rate -> UGen
brownNoise z r = mkOscId z r "BrownNoise" [] 1

-- | Clip noise.
clipNoise :: ID a => a -> Rate -> UGen
clipNoise z r = mkOscId z r "ClipNoise" [] 1

-- | Randomly pass or block triggers.
coinGate :: ID a => a -> UGen -> UGen -> UGen
coinGate z prob i = mkFilterId z "CoinGate" [prob, i] 1

-- | Random impulses in (-1, 1).
dust2 :: ID a => a -> Rate -> UGen -> UGen
dust2 z r density = mkOscId z r "Dust2" [density] 1

-- | Random impulse in (0,1).
dust :: ID a => a -> Rate -> UGen -> UGen
dust z r density = mkOscId z r "Dust" [density] 1

-- | Random value in exponential distribution.
expRand :: ID a => a -> UGen -> UGen -> UGen
expRand z lo hi = mkOscId z IR "ExpRand" [lo, hi] 1

-- | Gray noise.
grayNoise :: ID a => a -> Rate -> UGen
grayNoise z r = mkOscId z r "GrayNoise" [] 1

-- | Random integer in uniform distribution.
iRand :: ID a => a -> UGen -> UGen -> UGen
iRand z lo hi = mkOscId z IR "IRand" [lo, hi] 1

-- | Clip noise.
lfClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfClipNoise z r freq = mkOscId z r "LFClipNoise" [freq] 1

-- | Dynamic clip noise.
lfdClipNoise :: ID a => a -> Rate -> UGen -> UGen
lfdClipNoise z r freq = mkOscId z r "LFDClipNoise" [freq] 1

-- | Dynamic step noise.
lfdNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise0 z r freq = mkOscId z r "LFDNoise0" [freq] 1

-- | Dynamic ramp noise.
lfdNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise1 z r freq = mkOscId z r "LFDNoise1" [freq] 1

-- | Dynamic quadratic noise
lfdNoise2 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise2 z r freq = mkOscId z r "LFDNoise2" [freq] 1

-- | Dynamic cubic noise
lfdNoise3 :: ID a => a -> Rate -> UGen -> UGen
lfdNoise3 z r freq = mkOscId z r "LFDNoise3" [freq] 1

-- | Step noise.
lfNoise0 :: ID a => a -> Rate -> UGen -> UGen
lfNoise0 z r freq = mkOscId z r "LFNoise0" [freq] 1

-- | Ramp noise.
lfNoise1 :: ID a => a -> Rate -> UGen -> UGen
lfNoise1 z r freq = mkOscId z r "LFNoise1" [freq] 1

-- | Quadratic noise.
lfNoise2 :: ID a => a -> Rate -> UGen -> UGen
lfNoise2 z r freq = mkOscId z r "LFNoise2" [freq] 1

-- | Random value in skewed linear distribution.
linRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
linRand z lo hi m = mkOscId z IR "LinRand" [lo, hi, m] 1

-- | Random value in sum of n linear distribution.
nRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
nRand z lo hi n = mkOscId z IR "NRand" [lo, hi, n] 1

-- | Pink noise.
pinkNoise :: ID a => a -> Rate -> UGen
pinkNoise z r = mkOscId z r "PinkNoise" [] 1

-- | Random value in uniform distribution.
rand :: ID a => a -> UGen -> UGen -> UGen
rand z lo hi = mkOscId z IR "Rand" [lo, hi] 1

-- | Random value in exponential distribution on trigger.
tExpRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tExpRand z lo hi trig = mkFilterId z "TExpRand" [lo, hi, trig] 1

-- | Random integer in uniform distribution on trigger.
tIRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tIRand z lo hi trig = mkFilterId z "TIRand" [lo, hi, trig] 1

-- | Random value in uniform distribution on trigger.
tRand :: ID a => a -> UGen -> UGen -> UGen -> UGen
tRand z lo hi trig = mkFilterId z "TRand" [lo, hi, trig] 1

-- | Triggered windex.
tWindex :: ID a => a -> UGen -> UGen -> UGen -> UGen
tWindex z i n a = mkFilterMCEId z "TWindex" [i, n] a 1

-- | White noise.
whiteNoise :: ID a => a -> Rate -> UGen
whiteNoise z r = mkOscId z r "WhiteNoise" [] 1
