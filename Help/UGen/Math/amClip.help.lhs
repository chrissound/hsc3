amClip a b

0 when b <= 0, a*b when b > 0

> import Sound.SC3.ID

> let n = whiteNoise 'a' AR
> in audition (out 0 (amClip n (fSinOsc KR 1 0 * 0.2)))
