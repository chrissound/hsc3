module Sound.SC3.UGen.FFT where

import Sound.SC3.UGen.Rate (Rate(AR, KR))
import Sound.SC3.UGen.UGen (UGen, UId, mkOsc, mkOscUId, uniquify, zeroUId)

-- | Fast fourier transform.
fft :: UGen -> UGen -> UGen
fft buf i = mkOsc KR "FFT" [buf,i] 1 0

-- | Inverse Fast Fourier Transform.
ifft :: UGen -> UGen
ifft buf = mkOsc AR "IFFT" [buf] 1 0

-- | Strict convolution of two continuously changing inputs.
convolution :: UGen -> UGen -> UGen -> UGen
convolution i kernel frameSize = mkOsc AR "Convolution" [i, kernel, frameSize] 1 0

pv_Add :: UGen -> UGen -> UGen
pv_Add ba bb = mkOsc KR "PV_Add" [ba,bb] 1 0

-- | Shift and scale the bin positions.
pv_BinShift :: UGen -> UGen -> UGen -> UGen
pv_BinShift buf str shift = mkOsc KR "PV_BinShift" [buf,str,shift] 1 0

-- | Combine low and high bins from two inputs.
pv_BinWipe :: UGen -> UGen -> UGen -> UGen
pv_BinWipe ba bb wp = mkOsc KR "PV_BinWipe" [ba,bb,wp] 1 0

-- | Clear bins above or below a cutoff point.
pv_BrickWall :: UGen -> UGen -> UGen
pv_BrickWall buf wp = mkOsc KR "PV_BrickWall" [buf,wp] 1 0

pv_ConformalMap :: UGen -> UGen -> UGen -> UGen
pv_ConformalMap buf real imag = mkOsc KR "PV_ConformalMap" [buf,real,imag] 1 0

-- | Copies spectral frame.
pv_Copy :: UGen -> UGen -> UGen
pv_Copy ba bb = mkOsc KR "PV_Copy" [ba,bb] 1 0

pv_CopyPhase :: UGen -> UGen -> UGen
pv_CopyPhase ba bb = mkOsc KR "PV_CopyPhase" [ba,bb] 1 0

pv_Diffuser :: UGen -> UGen -> UGen
pv_Diffuser buf trg = mkOsc KR "PV_Diffuser" [buf,trg] 1 0

pv_HainsworthFoote :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_HainsworthFoote buf h f thr wait = mkOsc AR "PV_HainsworthFoote" [buf,h,f,thr,wait] 1 0

pv_JensenAndersen :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_JensenAndersen buf sc hfe hfc sf thr wait = mkOsc AR "PV_JensenAndersen" [buf,sc,hfe,hfc,sf,thr,wait] 1 0

pv_LocalMax :: UGen -> UGen -> UGen
pv_LocalMax buf thr = mkOsc KR "PV_LocalMax" [buf,thr] 1 0

pv_MagAbove :: UGen -> UGen -> UGen
pv_MagAbove buf thr = mkOsc KR "PV_MagAbove" [buf,thr] 1 0

pv_MagBelow :: UGen -> UGen -> UGen
pv_MagBelow buf thr = mkOsc KR "PV_MagBelow" [buf,thr] 1 0

pv_MagClip :: UGen -> UGen -> UGen
pv_MagClip buf thr = mkOsc KR "PV_MagClip" [buf,thr] 1 0

pv_MagFreeze :: UGen -> UGen -> UGen
pv_MagFreeze buf frz = mkOsc KR "PV_MagFreeze" [buf,frz] 1 0

pv_MagMul :: UGen -> UGen -> UGen
pv_MagMul ba bb = mkOsc KR "PV_MagMul" [ba,bb] 1 0

pv_MagNoise :: UGen -> UGen
pv_MagNoise buf = mkOsc KR "PV_MagNoise" [buf] 1 0

pv_MagShift :: UGen -> UGen -> UGen -> UGen
pv_MagShift buf str shift = mkOsc KR "PV_MagShift" [buf,str,shift] 1 0

pv_MagSmear :: UGen -> UGen -> UGen
pv_MagSmear buf bins = mkOsc KR "PV_MagSmear" [buf,bins] 1 0

pv_MagSquared :: UGen -> UGen
pv_MagSquared buf = mkOsc KR "PV_MagSquared" [buf] 1 0

pv_Max :: UGen -> UGen -> UGen
pv_Max ba bb = mkOsc KR "PV_Max" [ba,bb] 1 0

pv_Min :: UGen -> UGen -> UGen
pv_Min ba bb = mkOsc KR "PV_Min" [ba,bb] 1 0

pv_Mul :: UGen -> UGen -> UGen
pv_Mul ba bb = mkOsc KR "PV_Mul" [ba,bb] 1 0

pv_PhaseShift270 :: UGen -> UGen
pv_PhaseShift270 buf = mkOsc KR "PV_PhaseShift270" [buf] 1 0

pv_PhaseShift90 :: UGen -> UGen
pv_PhaseShift90 buf = mkOsc KR "PV_PhaseShift90" [buf] 1 0

pv_PhaseShift :: UGen -> UGen -> UGen
pv_PhaseShift buf shift = mkOsc KR "PV_PhaseShift" [buf,shift] 1 0

pv_RectComb2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb2 ba bb teeth phase width = mkOsc KR "PV_RectComb2" [ba,bb,teeth,phase,width] 1 0

pv_RectComb :: UGen -> UGen -> UGen -> UGen -> UGen
pv_RectComb buf teeth phase width = mkOsc KR "PV_RectComb" [buf,teeth,phase,width] 1 0

-- Local Variables:
-- truncate-lines:t
-- End:
