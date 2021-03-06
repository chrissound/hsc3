> Sound.SC3.UGen.Help.viewSC3Help "PV_BrickWall"
> Sound.SC3.UGen.DB.ugenSummary "PV_BrickWall"

> import Sound.SC3

> withSC3 (async (b_alloc 10 2048 1))

> let {n = whiteNoise 'α' AR
>     ;x = mouseX KR (-1) 1 Linear 0.1}
> in audition (out 0 (ifft' (pv_BrickWall (fft' 10 (n * 0.2)) x)))
