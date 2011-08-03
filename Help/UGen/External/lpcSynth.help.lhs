lpcSynth buffer excitation ptr
lpcVals rate buffer ptr

lpcSynth uses data from a LPC data file to filter a signal.
lpcVals returns pitch, rms and error data from the LPC data
file.

      buffer - the buffer LPC data is stored
         ptr - index into LPC data (0,1)
  excitation - the signal to filter

lpcVals reads LPC data and extracts frequency (cps),
amplitude (rmso) and error signals.

The LPC analysis files read are those generated by
lpanal (see csound).

Note: since the LPC data set will likely exceed the UDP packet
limit, load_data splits the LPC data into 512*4 byte packets.
Alternately use TCP, or write the LPC data to a disk file and use
b_allocRead.

> import Sound.SC3.Monadic

> let { load_data fd b i d =
>       if length d < 512
>       then send fd (b_setn1 b i d)
>       else do { send fd (b_setn1 b i (take 512 d))
>               ; load_data fd b (i + 512) (drop 512 d) }
>     ; lpc_instr b n lpc =
>       let { x = mouseX' KR 0.05 1.5 Linear 0.2
>           ; y = mouseY' KR 0.25 2.0 Linear 0.2
>           ; f = x / constant (lpcAnalysisDuration (lpcHeader lpc))
>           ; ptr = lfSaw AR f 1 * 0.5 + 0.5
>           ; MCE [cps, rms, err] = lpcVals AR b ptr
>           ; nh = floorE (22000 / cps)
>           ; voc = blip AR (cps * y) nh * (1 - err)
>           ; s = lpcSynth b (voc + (n * err * 20)) ptr }
>       in s * 1e-5 * rms }
> in do { lpc <- lpcRead "/home/rohan/cvs/tn/tn-56/lpc/fate.lpc"
>       ; n <- pinkNoise AR
>       ; let { d = lpcSC3 lpc
>             ; s = lpc_instr 10 n lpc }
>         in withSC3 (\fd -> do { async fd (b_alloc 10 (length d) 1)
>                               ; load_data fd 10 0 d
>                               ; play fd (out 0 s) }) }
