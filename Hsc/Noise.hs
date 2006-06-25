module Hsc.Noise where

import Hsc.Construct (mkOsc, mkOsc', uniquify)
import Hsc.UId       (UId(UId))

brownnoise' id r = mkOsc' r "BrownNoise" [] 1 0 id
clipnoise'  id r = mkOsc' r "ClipNoise"  [] 1 0 id
graynoise'  id r = mkOsc' r "GrayNoise"  [] 1 0 id
pinknoise'  id r = mkOsc' r "PinkNoise"  [] 1 0 id
whitenoise' id r = mkOsc' r "WhiteNoise" [] 1 0 id

lfclipnoise'  id r freq = mkOsc' r "LFClipNoise"  [freq] 1 0 id
lfnoise0'     id r freq = mkOsc' r "LFNoise0"     [freq] 1 0 id
lfnoise1'     id r freq = mkOsc' r "LFNoise1"     [freq] 1 0 id
lfnoise2'     id r freq = mkOsc' r "LFNoise2"     [freq] 1 0 id

lfdclipnoise' id r freq = mkOsc' r "LFDClipNoise" [freq] 1 0 id
lfdnoise0'    id r freq = mkOsc' r "LFDNoise0"    [freq] 1 0 id
lfdnoise1'    id r freq = mkOsc' r "LFDNoise1"    [freq] 1 0 id
lfdnoise2'    id r freq = mkOsc' r "LFDNoise2"    [freq] 1 0 id

dust'  id r density = mkOsc' r "Dust"  [density] 1 0 id
dust2' id r density = mkOsc' r "Dust2" [density] 1 0 id

texprand' id r lo hi trig = mkOsc' r "TExpRand" [lo,hi,trig] 1 0 id
tirand'   id r lo hi trig = mkOsc' r "TIRand"   [lo,hi,trig] 1 0 id
trand'    id r lo hi trig = mkOsc' r "TRand"    [lo,hi,trig] 1 0 id

coingate' id r prob i = mkOsc' r "CoinGate" [prob,i] 1 0 id

irand'   id r lo hi   = mkOsc' r "IRand"   [lo,hi]   1 0 id
nrand'   id r lo hi n = mkOsc' r "NRand"   [lo,hi,n] 1 0 id
rand'    id r lo hi   = mkOsc' r "Rand"    [lo,hi]   1 0 id
exprand' id r lo hi   = mkOsc' r "ExpRand" [lo,hi]   1 0 id
linrand' id r lo hi m = mkOsc' r "LinRand" [lo,hi,m] 1 0 id

brownnoise r = uniquify (brownnoise' (UId 0) r)
clipnoise r  = uniquify (clipnoise'  (UId 0) r)
graynoise r  = uniquify (graynoise'  (UId 0) r)
pinknoise r  = uniquify (pinknoise'  (UId 0) r)
whitenoise r = uniquify (whitenoise' (UId 0) r)

lfclipnoise r freq = uniquify (lfclipnoise'  (UId 0) r freq)
lfnoise0    r freq = uniquify (lfnoise0'     (UId 0) r freq)
lfnoise1    r freq = uniquify (lfnoise1'     (UId 0) r freq)
lfnoise2    r freq = uniquify (lfnoise2'     (UId 0) r freq)

dust  r density = uniquify (dust'   (UId 0) r density)
dust2 r density = uniquify (dust2'  (UId 0) r density)

texprand r lo hi trig = uniquify (texprand' (UId 0) r lo hi trig)
tirand   r lo hi trig = uniquify (tirand'   (UId 0) r lo hi trig)
trand    r lo hi trig = uniquify (trand'    (UId 0) r lo hi trig)

coingate r prob i = uniquify (coingate' (UId 0) r prob i)

irand   r lo hi   = uniquify (irand'   (UId 0) r lo hi)
nrand   r lo hi n = uniquify (nrand'   (UId 0) r lo hi n)
rand    r lo hi   = uniquify (rand'    (UId 0) r lo hi)
exprand r lo hi   = uniquify (exprand' (UId 0) r lo hi)
linrand r lo hi m = uniquify (linrand' (UId 0) r lo hi m)

hasher r i            = mkOsc r "Hasher"       [i]      1 0
mantissamask r i bits = mkOsc r "MantissaMask" [i,bits] 1 0
