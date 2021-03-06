    > Sound.SC3.UGen.Help.viewSC3Help "DetectIndex"
    > Sound.SC3.UGen.DB.ugenSummary "DetectIndex"

> import Sound.SC3 {- hsc3 -}

Allocate and set values at buffer ten

    > withSC3 (async (b_alloc_setn1 10 0 [2,3,4,0,1,5]))

Find indexes and map to an audible frequency range.

> g_01 =
>     let n = 6
>         x = floorE (mouseX KR 0 n Linear 0.1)
>         i = detectIndex 10 x
>     in sinOsc AR (linExp i 0 n 200 700) 0 * 0.1

Free buffer.

    > withSC3 (send (b_free 10))
