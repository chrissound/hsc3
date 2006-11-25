phasor trig rate start end resetPos

Triggered linear ramp between two levels.  Starts a linear ramp
when trig input crosses from non-positive to positive.

trig       - sets phase to resetPos (default: 0, equivalent to start)
rate       - rate value in 1 / frameDur (at 44.1 kHz sample rate: rate
             1 is eqivalent to 44100/sec)
start, end - start and end points of ramp
resetPos   - determines where to jump to on recieving a trigger.  the
             value at that position can be calculated as follows:
             (end - start) * resetPos

phasor controls sine frequency: end frequency matches a second sine wave.

> let rate = mouseX KR 0.2 2 Exponential 0.1
> let trig = impulse AR rate 0
> let sr   = sampleRate
> let x    = phasor AR trig (rate / sr) 0 1 0
> audition $ sinOsc AR (MCE [linLin x 0 1 600 1000, 1000]) 0 * 0.2