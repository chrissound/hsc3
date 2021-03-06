    > Sound.SC3.UGen.Help.viewSC3Help "GVerb"
    > Sound.SC3.UGen.DB.ugenSummary "GVerb"

> import Sound.SC3 {- hsc3 -}

> g_01 =
>     let i = impulse AR 1 0
>         c = lfCub AR 1200 0
>         s = decay i 0.25 * c * 0.1
>     in gVerb s 10 3 0.5 0.5 15 1 0.7 0.5 300
