thresh a b

Signal thresholding.  0 when a < b, otherwise a.

> n <- lfNoise0 AR 50
> audition $ thresh (n * 0.5) 0.45