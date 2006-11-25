resonz in freq bwr

Resonant filter.

A two pole resonant filter with zeroes at z = +/- 1. Based on
K. Steiglitz, "A Note on Constant-Gain Digital Resonators,"
Computer Music Journal, vol 18, no. 4, pp. 8-10, Winter 1994.  The
reciprocal of Q is used rather than Q because it saves a divide
operation inside the unit generator.

in - input signal to be processed
freq - resonant frequency in Hertz
rq - bandwidth ratio (reciprocal of Q). rq = bandwidth / centerFreq

> resonz AR (whitenoise 0 AR * 0.5) 2000 0.1

Modulate frequency

> resonz AR (whitenoise 0 AR * 0.5) (xline KR 1000 8000 10 2) 0.05

Modulate bandwidth

> resonz AR (whitenoise 0 AR * 0.5) 2000 (xline KR 1 0.001 8 2)

Modulate bandwidth opposite direction

> resonz AR (whitenoise 0 AR * 0.5) 2000 (xline KR 0.001 1 8 2)