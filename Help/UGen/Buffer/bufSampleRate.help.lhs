bufSampleRate bufnum

Buffer sample rate.

> withSC3 (\fd -> do send fd (b_allocRead 0 "/home/rohan/sw/sw-01/audio/metal.wav" 0 0)
>                    wait fd "/done")

Compare a sine tone derived from sample rate of a buffer with a
440Hz tone.

> let f = MCE [bufSampleRate KR 0 * 0.01, 440]
> audition $ sinOsc AR f 0 * 0.1
