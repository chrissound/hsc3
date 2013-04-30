> Sound.SC3.UGen.Help.viewSC3Help "Env.*adsr"
> :i Sound.SC3.ADSR
> :t Sound.SC3.envADSR_r
> :t Sound.SC3.envADSR

> import Sound.SC3

> let {g = control KR "gate" 1
>     ;p = envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>     ;e = envGen KR g 0.1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (send (n_set1 (-1) "gate" 0))
> withSC3 (send (n_set1 (-1) "gate" 1))
> withSC3 (send (n_free [-1]))

> import Sound.SC3.Plot

> plotEnvelope [envADSR 0.75 0.75 0.5 0.75 1 (EnvNum (-4)) 0
>              ,envADSR 0.02 0.2 0.25 1 1 (EnvNum (-4)) 0
>              ,envADSR 0.001 0.2 0.25 1 1 (EnvNum (-4)) 0
>              ,envADSR 0 2 1 0.1 0.5 EnvSin 0
>              ,envADSR 0.001 1.54 1 0.001 0.4 EnvSin 0]

There is a record variant:

> let {g = control KR "gate" 1
>     ;c = EnvNum (-4)
>     ;r = ADSR {adsr_attackTime = 0.75
>               ,adsr_decayTime = 0.75
>               ,adsr_sustainLevel = 0.5
>               ,adsr_releaseTime = 0.75
>               ,adsr_peakLevel = 1
>               ,adsr_curve = (c,c,c)
>               ,adsr_bias = 0}
>     ;p = envADSR_r r
>     ;e = envGen KR g 0.1 0 1 DoNothing p}
> in audition (out 0 (sinOsc AR 440 0 * e))

> withSC3 (send (n_set1 (-1) "gate" 0))
> withSC3 (send (n_set1 (-1) "gate" 1))
> withSC3 (send (n_free [-1]))
