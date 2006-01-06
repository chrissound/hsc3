lfpulse freq iphase width

A non-band-limited pulse oscillator. Outputs a high value of one
and a low value of zero.  Note that the iphase argument was not
present in SC2.

freq - frequency in Hertz
iphase - initial phase offset in cycles ( 0..1 )
width - pulse width duty cycle from zero to one.

> lfpulse AR (lfpulse KR 3 0 0.3 * 200 + 200) 0 0.2 * 0.1