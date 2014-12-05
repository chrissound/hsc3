> Sound.SC3.UGen.Help.viewSC3Help "COsc"
> Sound.SC3.UGen.DB.ugenSummary "COsc"

> import Sound.SC3

Allocate and fill buffer.

> let {b = 10;f = [Normalise,Wavetable,Clear];p = [1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10]}
> in withSC3 (async (b_alloc b 512 1) >> async (b_gen_sine1 b f p))

Fixed beat frequency

> audition (out 0 (cOsc AR 10 200 0.7 * 0.1))

Modulate beat frequency with mouseX

> audition (out 0 (cOsc AR 10 200 (mouseX KR 0 4 Linear 0.2) * 0.1))

Compare with plain osc

> audition (out 0 (osc AR 10 200 0.0 * 0.1))

Summing behaviour (http://article.gmane.org/gmane.comp.audio.supercollider.devel/62575)

> import Sound.SC3.Plot {- hsc3-plot -}

> let {b = 11; f = [Normalise,Wavetable,Clear]; p = [1]}
> in withSC3 (async (b_alloc b 512 1) >> async (b_gen_sine1 b f p))

> plot_ugen1 0.1 (cOsc AR 11 100 5)
