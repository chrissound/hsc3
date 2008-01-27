analog bubbles

> let { o = lfSaw KR (mce2 8 7.23) 0 * 3 + 80
>     ; f = lfSaw KR 0.4 0 * 24 + o
>     ; s = sinOsc AR (midiCPS f) 0 * 0.04 }
> in audition (out 0 (combN s 0.2 0.2 4))

{ var o = LFSaw.kr([8, 7.23], 0, 3, 80)
; var f = LFSaw.kr(0.4, 0, 24, o)
; var s = SinOsc.ar(f.midicps, 0, 0.04)
; Out.ar(0, CombN.ar(s, 0.2, 0.2, 4)) }.play