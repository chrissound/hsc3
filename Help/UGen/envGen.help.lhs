    > Sound.SC3.UGen.Help.viewSC3Help "EnvGen"
    > Sound.SC3.UGen.DB.ugenSummary "EnvGen"

At least the following envelope constructors are provided:
envPerc, envSine, envCoord, envTrapezoid, and envLinen.

> import Sound.SC3 {- hsc3 -}

env_circle joins the end of the envelope to the start

> g_01 =
>     let e = Envelope [6000,700,100] [1,1] [EnvExp,EnvLin] Nothing Nothing
>         f = envGen KR 1 1 0 1 DoNothing (env_circle e 0 EnvLin)
>     in sinOsc AR f 0 * 0.1 + impulse AR 1 0

Env([6000,700,100],[1,1],['exp','lin']).circle.asArray == [6000,2,-99,-99,700,1,2,0,100,1,1,0]

    > let {e = Envelope [6000,700,100] [1,1] [EnvExp,EnvLin] Nothing Nothing
    >     ;r = [0,4,3,0,6000,0,1,0,700,1,2,0,100,1,1,0,0,9e8,1,0]}
    > in envelope_sc3_array (env_circle e 0 EnvLin) == Just r

Env([0,1],[0.1]).asArray == [0,1,-99,-99,1,0.1,1,0]

    > let e = (Envelope [0,1] [0.1] [EnvLin] Nothing Nothing)
    > in envelope_sc3_array e == Just [0,1,-99,-99,1,0.1,1,0]

https://www.listarc.bham.ac.uk/lists/sc-users/msg14815.html

> g_02 =
>     let n = range 0.01 0.1 (lfNoise1 'α' KR 2)
>         e = Envelope [0,1] [n] [EnvLin] Nothing (Just 0)
>         a = envGen AR 1 1 0 1 DoNothing (env_circle e 0 EnvLin)
>     in sinOsc AR (a * 400 + 500) 0 * 0.1
