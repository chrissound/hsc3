-- Sound.SC3.UGen.Help.viewSC3Help "BlitB3Saw"
-- Sound.SC3.UGen.DB.ugenSummary "BlitB3Saw"

import Sound.SC3 {- hsc3 -}

-- audition (out 0 sw1)
sw1 =
    let f = xLine KR 1000 20 10 DoNothing
    in blitB3Saw AR f 0.99 * 0.1

-- aliasing suddenly appears for very high frequencies
--
-- audition (out 0 sw2)
sw2 =
    let f = mouseX KR 10 10000 Exponential 0.2
        c = mouseY KR 0.01 0.99 Linear 0.2
    in blitB3Saw AR f c * 0.1

-- comparison
--
-- audition (out 0 (sw3 20))
sw3 f = mce2 (saw AR f) (blitB3Saw AR f 0.99) * 0.1
