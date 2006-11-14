module Sound.SC3.UGen.Panner where

import Sound.SC3.UGen.UGen (UGen, mkFilter, mkFilterMCE)

pan2 :: UGen -> UGen -> UGen -> UGen
pan2 i x level = mkFilter "Pan2" [i, x, level] 2 0

linPan2 :: UGen -> UGen -> UGen -> UGen
linPan2 i x level = mkFilter "LinPan2" [i, x, level] 2 0

pan4 :: UGen -> UGen -> UGen -> UGen -> UGen
pan4 i x y level = mkFilter "Pan4" [i, x, y, level] 4 0

balance2 :: UGen -> UGen -> UGen -> UGen -> UGen
balance2 l r p level = mkFilter "Balance2" [l, r, p, level] 2 0 

rotate2 :: UGen -> UGen -> UGen -> UGen
rotate2 x y pos = mkFilter "Rotate2" [x, y, pos] 2 0

panB :: UGen -> UGen -> UGen -> UGen -> UGen
panB i az el level = mkFilter "PanB" [i, az, el, level] 4 0

panB2 :: UGen -> UGen -> UGen -> UGen
panB2 i az level = mkFilter "PanB2" [i, az, level] 3 0

biPanB2 :: UGen -> UGen -> UGen -> UGen -> UGen
biPanB2 inA inB azimuth gain = mkFilter "BiPanB2" [inA, inB, azimuth, gain] 3 0

decodeB2 :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen
decodeB2 nc w x y o = mkFilterMCE "DecodeB2" [w, x, y, o] nc 0 0

panAz :: UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen
panAz nc i p l w o = mkFilterMCE "PanAz" [i, p, l, w, o] nc 0 0

xFade2 :: UGen -> UGen -> UGen -> UGen -> UGen
xFade2 inA inB pan level = mkFilter "XFade" [inA, inB, pan, level] 2 0

linXFade2 :: UGen -> UGen -> UGen -> UGen
linXFade2 inA inB pan = mkFilter "XFade" [inA, inB, pan] 2 0