-- Sound.SC3.UGen.Help.viewSC3Help "PV_Copy"
-- Sound.SC3.UGen.DB.ugenSummary "PV_Copy"

import Sound.SC3.ID

{-|

Proof of concept, silence.  This is very fragile.
If c0 and c1 are swapped the output is no longer
silence.  pv_Copy is required to run before the
ifft of c0, which mutates c0, however that is not
apparent from the wire structure of the graph.  In
the case of cpy0 the graph happens to sort as
required, in cpy1 it does not.

> audition (out 0 cpy0)
-}
cpy0 =
    let z = lfClipNoise 'α' AR 100 * 0.1
        c0 = fft' (localBuf 'β' 2048 1) z
        c1 = pv_Copy c0 (localBuf 'γ' 2048 1)
    in ifft' c1 - ifft' c0

-- > audition (out 0 cpy1)
cpy1 =
    let z = lfClipNoise 'α' AR 100 * 0.1
        c0 = fft' (localBuf 'β' 2048 1) z
        c1 = pv_Copy c0 (localBuf 'γ' 2048 1)
    in ifft' c0 - ifft' c1

{-
The equivalent situation in sclang.

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;IFFT(g) - IFFT(PV_Copy(g,LocalBuf(2048)))}.play

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;IFFT(PV_Copy(g,LocalBuf(2048))) - IFFT(g)}.play

{var z = LFClipNoise.ar(100) * 0.1
;var g = FFT(LocalBuf(2048),z)
;var h = PV_Copy(g,LocalBuf(2048))
;IFFT(g) - IFFT(h)}.play

-}
