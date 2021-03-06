    Sound.SC3.UGen.Help.viewSC3Help "TDelay"
    Sound.SC3.UGen.DB.ugenSummary "TDelay"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let z = impulse AR 2 0
>         z' = tDelay z 0.5
>         o = sinOsc AR 440 0 * 0.1
>     in mce [z * 0.1,toggleFF z' * o]
