> Sound.SC3.UGen.Help.viewSC3Help "StkFlute"
> Sound.SC3.UGen.DB.ugenSummary "StkFlute"

> import Sound.SC3

> let { bp = line KR 76 32 3 RemoveSynth
>     ; ng = line KR 16 64 3 DoNothing }
> in audition (out 0 (stkFlute AR 400 64 ng 16 16 bp 1))
