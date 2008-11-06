scritto (rd)

> import Sound.SC3

> let { scritto = [ ( "sA" 
>                   , [800, 1150, 2900, 3900, 4950]
>                   , [0, -6,  -32, -20, -50]
>                   , [80, 90,  120, 130, 140] )
>                 , ( "sE" 
>                   , [350, 2000, 2800, 3600, 4950]
>                   , [0, -20, -15, -40, -56]
>                   , [60, 100, 120, 150, 200] )
>                 , ( "sI" 
>                   , [270, 2140, 2950, 3900, 4950]
>                   , [0, -12 , -26 , -26 , -44]
>                   , [60, 90,  100, 120, 120] )
>                 , ( "sO" 
>                   , [450, 800,  2830, 3800, 4950]
>                   , [0, -11 , -22 , -22 , -50]
>                   , [70, 80,  100, 130, 135] )
>                 , ( "sU" 
>                   , [325, 700,  2700, 3800, 4950]
>                   , [0, -16, -35, -40, -60]
>                   , [50, 60,  170, 180, 200] )
>                 , ( "aA" 
>                   , [800, 1150, 2800, 3500, 4950]
>                   , [0, -4  , -20, -36 , -60]
>                   , [80, 90,  120, 130, 140] )
>                 , ( "aE" 
>                   , [400, 1600, 2700, 3300, 4950]
>                   , [0, -24 , -30, -35, -60]
>                   , [60, 80,  120, 150, 200] )
>                 , ( "aI" 
>                   , [350, 1700, 2700, 3700, 4950]
>                   , [0, -20, -30, -36 , -60]
>                   , [50, 100, 120, 150, 200] )
>                 , ( "aO" 
>                   , [450, 800,  2830, 3500, 4950]
>                   , [0, -9  , -16 , -28 , -55]
>                   , [70, 80,  100, 130, 135] )
>                 , ( "aU" 
>                   , [325, 700, 2530, 3500, 4950]
>                   , [0, -12 , -30, -40, -64]
>                   , [50, 60,  170, 180, 200] )
>                 , ( "ctA"
>                   , [660, 1120, 2750, 3000, 3350]
>                   , [0, -6  , -23 , -24 , -38]
>                   , [80, 90,  120, 130, 140] )
>                 , ( "ctE"
>                   , [440, 1800, 2700, 3000, 3300]
>                   , [0, -14 , -18 , -20, -20]
>                   , [70, 80,  100, 120, 120] )
>                 , ( "ctI"
>                   , [270, 1850, 2900, 3350, 3590]
>                   , [0, -24 , -24 , -36 , -36]
>                   , [40, 90,  100, 120, 120] )
>                 , ( "ctO"
>                   , [430, 820,  2700, 3000, 3300]
>                   , [0, -10, -26 , -22 , -34]
>                   , [40, 80,  100, 120, 120] )
>                 , ( "ctU"
>                   , [370, 630,  2750, 3000, 3400]
>                   , [0, -20, -23 , -30, -34]
>                   , [40, 60,  100, 120, 120] )
>                 , ( "tA" 
>                   , [650, 1080, 2650, 2900, 3250]
>                   , [0, -6   , -7  , -8 , -22]
>                   , [80, 90,  120, 130, 140] )
>                 , ( "tE" 
>                   , [400, 1700, 2600, 3200, 3580]
>                   , [0, -14 , -12 , -14 , -20]
>                   , [70, 80,  100, 120, 120] )
>                 , ( "tI" 
>                   , [290, 1870, 2800, 3250, 3540]
>                   , [0, -15, -18 , -20, -30]
>                   , [40, 90,  100, 120, 120] )
>                 , ( "tO" 
>                   , [400, 800,  2600, 2800, 3000]
>                   , [0, -10, -12 , -12 , -26]
>                   , [40, 80,  100, 120, 120] )
>                 , ( "tU" 
>                   , [350, 600,  2700, 2900, 3300]
>                   , [0, -20, -17 , -14 , -26]
>                   , [40, 60,  100, 120, 120] )
>                 , ( "bA" 
>                   , [600, 1040, 2250, 2450, 2750]
>                   , [0, -7   , -9  , -9 , -20]
>                   , [60, 70,  110, 120, 130] )
>                 , ( "bE" 
>                   , [400, 1620, 2400, 2800, 3100]
>                   , [0, -12  , -9 , -12 , -18]
>                   , [40, 80,  100, 120, 120] )
>                 , ( "bI" 
>                   , [250, 1750, 2600, 3050, 3340]
>                   , [0, -30, -16 , -22 , -28]
>                   , [60, 90,  100, 120, 120] )
>                 , ( "bO" 
>                   , [400, 750,  2400, 2600, 2900]
>                   , [0, -11 , -21 , -20, -40]
>                   , [40, 80,  100, 120, 120] )
>                 , ( "bU" 
>                   , [350, 600,  2400, 2675, 2950]
>                   , [0, -20, -32 , -28 , -36]
>                   , [40, 80,  100, 120, 120] ) ]
>     ; s_msg n (_, f, a, b) = b_setn1 n 0 (f ++ a ++ b)
>     ; s_alloc fd (s, b) = do { async fd (b_alloc b 15 1)
>                              ; send fd (s_msg b s) }
>     ; buf_at b n = bufRd 1 kr b (mce [n .. n + 4]) NoLoop NoInterpolation
>     ; v_filter i f a b = resonz i f (b / f) * dbAmp a
>     ; v_filter_b bi i = v_filter i (buf_at bi 0) (buf_at bi 5) (buf_at bi 10)
>     ; mk_instr bx = do 
>         { n <- lfNoise2 kr 3
>         ; let { t = impulse ar (n * 9 + 9) 0
>               ; i d = do { n1 <- tRand 0.02 0.06 t
>                          ; n2 <- tiRand 30 52 t
>                          ; n3 <- tiRand 16 32 t
>                          ; let { p = pulseDivider t d 0
>                                ; b = blip ar (midiCPS n2) n3 }
>                            in return (decay2 p 0.01 n1 * b * 12) }
>               ; bi = linLin n (-1) 1 0 bx
>               ; voice = mix . v_filter_b bi }
>           in return . out 0 . mce . map voice =<< mapM i [1, 2] } }
> in withSC3 (\fd -> do { mapM_ (s_alloc fd) (zip scritto [0..])
>                       ; let n = constant (length scritto)
>                         in audition =<< mk_instr n })
