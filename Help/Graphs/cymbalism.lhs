cymbalism (jmcc)

> let p = 15
> in do { f1 <- rand 500 2500
>       ; f2 <- rand 0 8000
>       ; let y = do { f <- replicateM p (rand f1 (f1 + f2))
>                    ; rt <- replicateM p (rand 1 5)
>                    ; return (klankSpec f (replicate p 1) rt) }
>         in do { z <- clone 2 y
>               ; n <- liftM (* 0.03) (whiteNoise AR)
>               ; tf <- rand 0.5 3.5
>               ; let { t = impulse AR tf 0
>                     ; s = decay t 0.004 * n
>                     ; k = klank s 1 0 1 (mceTranspose z) }
>                 in audition (out 0 k) } }

{ var p = 15
; var f1 = Rand.new(500, 2500)
; var f2 = Rand.new(0, 8000)
; var y = { var f = Array.fill(p, { f1 + Rand.new(0, f2) } )
          ; var rt = Array.fill(p, { 1 + Rand.new(0, 4) })
          ; `[f, nil, rt] }
; var z = Array.fill(2, y)
; var t = Impulse.ar(Rand.new(0, 3) + 0.5, 0)
; var n = WhiteNoise.ar() * 0.03
; var s = Decay.ar(t, 0.004) * n
; Out.ar(0, Klank.ar(z, s)) }.play
