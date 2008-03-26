wial (rd)

> let { pls c d f = let { t = pulseDivider c d 0
>                       ; e = decay2 t 0.05 0.75
>                       ; o = sinOsc AR (toggleFF t * f + f * 2) 0 }
>                    in do { n0 <- tiRand 0 1 t
>                          ; return (o * e * n0 * 0.5) }
>     ; smpl f = [ (4, 6, f      ,  0.75)
>                , (2, 6, f *   2,  0.75)
>                , (1, 2, f *  16,  0.025)
>                , (1, 5, f *  64,  0.005)
>                , (1, 4, f * 128,  0.035)
>                , (1, 3, f * 256,  0.15)
>                , (2, 3, f * 512,  0.35) ]
>     ; plss c (d0, d1, f, a) = liftM (* a) (pls c (mce2 d0 d1) f)
>     ; clk = impulse AR 16 0
>     ; twchoose t a w n = select (twindex t n w) a }
> in do { n0 <- dust KR 2
>       ; let f = twchoose n0 (mce2 (20 * 0.66) 20) (mce2 0.25 0.75) 0
>         in audition . out 0 . sum =<< mapM (plss clk) (smpl f) }
