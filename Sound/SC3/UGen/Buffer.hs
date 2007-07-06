module Sound.SC3.UGen.Buffer where

import Sound.SC3.UGen.Rate (Rate(AR))
import Sound.SC3.UGen.UGen (UGen, mkFilter, mkFilterMCE, mkOsc, mkOscMCE)
import Sound.SC3.UGen.Enum (Loop, Interpolation(..))
import Sound.SC3.UGen.Private (fromLoop, fromInterpolation)

-- * Buffer query UGens.

-- | Buffer channel count.
bufChannels :: Rate -> UGen -> UGen
bufChannels r buf = mkOsc r "BufChannels" [buf] 1

-- | Buffer duration, in seconds.
bufDur :: Rate -> UGen -> UGen
bufDur r buf = mkOsc r "BufDur" [buf] 1

-- | Buffer frame count.
bufFrames :: Rate -> UGen -> UGen
bufFrames r buf = mkOsc r "BufFrames" [buf] 1

-- | Buffer rate scalar with respect to server sample rate.
bufRateScale :: Rate -> UGen -> UGen
bufRateScale r buf = mkOsc r "BufRateScale" [buf] 1

-- | Buffer sample rate.
bufSampleRate :: Rate -> UGen -> UGen
bufSampleRate r buf = mkOsc r "BufSampleRate" [buf] 1

-- | Buffer sample count (ie. frame count by channel count).
bufSamples :: Rate -> UGen -> UGen
bufSamples r buf = mkOsc r "BufSamples" [buf] 1

-- * Buffer filters and delays.

-- | Allpass filter (cubic interpolation).
bufAllpassC :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassC buf i dly dcy = mkFilter "BufAllpassC" [buf,i,dly,dcy] 1

-- | Allpass filter (linear interpolation).
bufAllpassL :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassL buf i dly dcy = mkFilter "BufAllpassL" [buf,i,dly,dcy] 1

-- | Allpass filter (no interpolation).
bufAllpassN :: UGen -> UGen -> UGen -> UGen -> UGen
bufAllpassN buf i dly dcy = mkFilter "BufAllpassN" [buf,i,dly,dcy] 1

-- | Comb filter (cubic interpolation).
bufCombC :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombC buf i dly dcy = mkFilter "BufCombC" [buf,i,dly,dcy] 1

-- | Comb filter (linear interpolation).
bufCombL :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombL buf i dly dcy = mkFilter "BufCombL" [buf,i,dly,dcy] 1

-- | Comb filter (no interpolation).
bufCombN :: UGen -> UGen -> UGen -> UGen -> UGen
bufCombN buf i dly dcy = mkFilter "BufCombN" [buf,i,dly,dcy] 1

-- | Delay line (cubic interpolation).
bufDelayC :: UGen -> UGen -> UGen -> UGen
bufDelayC buf i dly = mkFilter "BufDelayC" [buf,i,dly] 1

-- | Delay line (linear interpolation).
bufDelayL :: UGen -> UGen -> UGen -> UGen
bufDelayL buf i dly = mkFilter "BufDelayL" [buf,i,dly] 1

-- | Delay line (no interpolation).
bufDelayN :: UGen -> UGen -> UGen -> UGen
bufDelayN buf i dly = mkFilter "BufDelayN" [buf,i,dly] 1

-- * Buffer I\/O.

-- | Buffer reader.
bufRd :: Int -> Rate -> UGen -> UGen -> Loop -> Interpolation -> UGen
bufRd n r buf phs lp intp = mkOsc r "BufRd" [buf,phs,fromLoop lp,fromInterpolation intp] n

-- | Buffer reader (no interpolation).
bufRdN :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdN n r b p l = bufRd n r b p l NoInterpolation

-- | Buffer reader (linear interpolation).
bufRdL :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdL n r b p l = bufRd n r b p l LinearInterpolation

-- | Buffer reader (cubic interpolation).
bufRdC :: Int -> Rate -> UGen -> UGen -> Loop -> UGen
bufRdC n r b p l = bufRd n r b p l CubicInterpolation

-- | Buffer writer.
bufWr :: UGen -> UGen -> Loop -> UGen -> UGen
bufWr buf phs lp i = mkFilterMCE "BufWr" [buf,phs,fromLoop lp] i 0

-- | Index into table with signal.
index :: UGen -> UGen -> UGen
index b i = mkFilter "Index" [b, i] 1

-- | Buffer playback.
playBuf :: Int -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen
playBuf n b r' t s l = mkOsc AR "PlayBuf" [b,r',t,s,fromLoop l] n

-- | Buffer recording.
recordBuf :: UGen -> UGen -> UGen -> UGen -> UGen -> Loop -> UGen -> UGen -> UGen
recordBuf b o rl pl r l t i = mkOscMCE AR "RecordBuf" [b, o, rl, pl, r, fromLoop l, t] i 0

-- | Triggered buffer shuffler (grain generator).
tGrains :: Int -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
tGrains n t b r c d p a i = mkFilter "TGrains" [t,b,r,c,d,p,a,i] n

-- Local Variables:
-- truncate-lines:t
-- End:
