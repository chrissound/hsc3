> Sound.SC3.UGen.Help.viewSC3Help "BufCombC"
> Sound.SC3.UGen.DB.ugenSummary "BufCombC"

> import Sound.SC3.ID

Allocate buffer zero (required for examples below)
> withSC3 (\fd -> async fd (b_alloc 0 44100 1))

Filtered decaying noise bursts
> let {d = dust 'a' AR 1
>     ;n = whiteNoise 'a' AR
>     ;x = decay d 0.2 * n * 0.25}
> in audition (out 0 (bufCombC 0 x 0.25 6))

Comb filter as resonator. The resonant fundamental is equal to
reciprocal of the delay time.
> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (bufCombC 0 (n * 0.1) dt 0.2))

With negative feedback
> let {n = whiteNoise 'a' AR
>     ;dt = xLine KR 0.0001 0.01 20 RemoveSynth}
> in audition (out 0 (bufCombC 0 (n * 0.1) dt (-0.2)))

Used as an echo.
> let {d = dust 'a' AR 1
>     ;n = whiteNoise 'a' AR
>     ;i = decay (d * 0.5) 0.2 * n}
> in audition (out 0 (bufCombC 0 i 0.2 3))
