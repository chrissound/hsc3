> Sound.SC3.UGen.Help.viewSC3Help "FFT"
> Sound.SC3.UGen.DB.ugenSummary "FFT"
> :t fft'

> import Sound.SC3

Non-local buffer

> withSC3 (async (b_alloc 10 2048 1))

Variants with default values

> let n = whiteNoise 'α' AR
> in audition (out 0 (ifft' (fft' 10 (n * 0.05))))

Local buffer allocating fft variant

> let {s0 = sinOsc KR 0.08 0 * 6 + 6.2
>     ;s1 = sinOsc KR (squared s0) 0 * 100 + 800
>     ;s2 = sinOsc AR s1 0}
> in audition (out 0 (ifft (ffta 'α' 2048 s2 0.5 0 1 0) 0 0 * 0.25))
