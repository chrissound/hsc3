> Sound.SC3.UGen.Help.viewSC3Help "HPZ1"
> Sound.SC3.UGen.DB.ugenSummary "HPZ1"

> import Sound.SC3

> let n = whiteNoise 'α' AR
> in audition (out 0 (hpz1 (n * 0.25)))
