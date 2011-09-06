> Sound.SC3.UGen.Help.viewSC3Help "PV_ChainUGen.pvcollect"
> :t pvcollect

> import Sound.SC3.ID

> let fileName = "/home/rohan/data/audio/pf-c5.snd"
> in withSC3 (\fd -> do { async fd (b_alloc 10 1024 1)
>                       ; async fd (b_allocRead 11 fileName 0 0) })

> let {no_op m p _ = (m,p)
>     ;combf m p i = ((fmod i 7.0 ==* 0) * m,p)
>     ;spectral_delay m p _ =
>      let {l = lfPar KR 0.5 0
>          ;v = linLin l (-1) 1 0.1 1}
>      in (m + delayN m 1 v,p)
>     ;nf = 1024
>     ;bpf_sweep m p i =
>      let {l = lfPar KR 0.1 0
>          ;e = abs (i - (linLin l (-1) 1 2 (nf / 20)))}
>      in ((e <* 10) * m,p)
>     ;sf = playBuf 1 AR 11 (bufRateScale KR 11) 1 0 Loop DoNothing
>     ;c1 = fft' 10 sf
>     ;c2 = pvcollect c1 nf spectral_delay 0 250 0}
> in audition (out 0 (0.1 * ifft' c2))
