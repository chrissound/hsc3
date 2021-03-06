    > Sound.SC3.UGen.Help.viewSC3Help "Drand"
    > Sound.SC3.UGen.DB.ugenSummary "Drand"

> import Sound.SC3 {- hsc3 -}
>
> g_01 =
>     let n = drand 'α' dinf (mce [1, 3, 2, 7, 8])
>         x = mouseX KR 1 400 Exponential 0.1
>         t = impulse KR x 0
>         f = demand t 0 n * 30 + 340
>     in sinOsc AR f 0 * 0.1
