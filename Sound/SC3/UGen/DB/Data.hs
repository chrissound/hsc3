-- AUTOGENERATED: 2007-08-08-17h04
module Sound.SC3.UGen.DB.Data where
import Sound.SC3.UGen.Rate
data I = I String Double deriving (Eq, Show)
data U = U String [Rate] [I] deriving (Eq, Show)
inf :: Double
inf = 10 ** 8
ugenDB :: [U]
ugenDB = [ U "A2K" [ KR ] [ I "in" (0) ]
         , U "APF" [ AR, KR ] [ I "in" (0), I "freq" (440), I "radius" (0.8) ]
         , U "AbstractIn" [  ] [ I "maxSize" (0) ]
         , U "AbstractOut" [  ] [ I "maxSize" (0) ]
         , U "AllpassC" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "AllpassL" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "AllpassN" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "AmpComp" [ AR, KR, IR ] [ I "freq" (0), I "root" (0), I "exp" (0.3333) ]
         , U "AmpCompA" [ AR, KR, IR ] [ I "freq" (1000), I "root" (0), I "minAmp" (0.32), I "rootAmp" (1) ]
         , U "Amplitude" [ AR, KR ] [ I "in" (0), I "attackTime" (0.01), I "releaseTime" (0.01) ]
         , U "BPF" [ AR, KR ] [ I "in" (0), I "freq" (440), I "rq" (1) ]
         , U "BPZ2" [ AR, KR ] [ I "in" (0) ]
         , U "BRF" [ AR, KR ] [ I "in" (0), I "freq" (440), I "rq" (1) ]
         , U "BRZ2" [ AR, KR ] [ I "in" (0) ]
         , U "Balance2" [ AR, KR ] [ I "left" (0), I "right" (0), I "pos" (0), I "level" (1) ]
         , U "Ball" [ AR ] [ I "in" (0), I "g" (1), I "damp" (0), I "friction" (0.01) ]
         , U "BasicOpUGen" [  ] [ I "maxSize" (0) ]
         , U "BiPanB2" [ AR, KR ] [ I "inA" (0), I "inB" (0), I "azimuth" (0), I "gain" (1) ]
         , U "BinaryOpUGen" [  ] [ I "selector" (0), I "a" (0), I "b" (0) ]
         , U "Blip" [ AR ] [ I "freq" (440), I "numharm" (200) ]
         , U "BrownNoise" [ AR, KR ] [  ]
         , U "BufAllpassC" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufAllpassL" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufAllpassN" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufChannels" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufCombC" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufCombL" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufCombN" [ AR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "BufDelayC" [ AR, KR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2) ]
         , U "BufDelayL" [ AR, KR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2) ]
         , U "BufDelayN" [ AR, KR ] [ I "buf" (0), I "in" (0), I "delaytime" (0.2) ]
         , U "BufDur" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufFrames" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufInfoUGenBase" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufRateScale" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufRd" [ AR, KR ] [ I "numChannels" (0), I "bufnum" (0), I "phase" (0), I "loop" (1), I "interpolation" (2) ]
         , U "BufSampleRate" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufSamples" [ KR, IR ] [ I "bufnum" (0) ]
         , U "BufWr" [ AR, KR ] [ I "inputArray" (0), I "bufnum" (0), I "phase" (0), I "loop" (1) ]
         , U "COsc" [ AR, KR ] [ I "bufnum" (0), I "freq" (440), I "beats" (0.5) ]
         , U "ChaosGen" [  ] [ I "maxSize" (0) ]
         , U "Clip" [ AR, KR ] [ I "in" (0), I "lo" (0), I "hi" (1) ]
         , U "ClipNoise" [ AR, KR ] [  ]
         , U "CoinGate" [ AR, KR ] [ I "prob" (0), I "in" (0) ]
         , U "CombC" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "CombL" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "CombN" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2), I "decaytime" (1) ]
         , U "Compander" [ AR ] [ I "in" (0), I "control" (0), I "thresh" (0.5), I "slopeBelow" (1), I "slopeAbove" (1), I "clampTime" (0.01), I "relaxTime" (0.1) ]
         , U "CompanderD" [ AR ] [ I "in" (0), I "thresh" (0.5), I "slopeBelow" (1), I "slopeAbove" (1), I "clampTime" (0.01), I "relaxTime" (0.01) ]
         , U "Control" [ KR, IR ] [ I "values" (0) ]
         , U "ControlRate" [ IR ] [  ]
         , U "Convolution" [ AR ] [ I "in" (0), I "kernel" (0), I "framesize" (512) ]
         , U "Convolution2" [ AR ] [ I "in" (0), I "bufnum" (0), I "trigger" (0), I "framesize" (0) ]
         , U "Convolution2L" [ AR ] [ I "in" (0), I "bufnum" (0), I "trigger" (0), I "framesize" (0), I "crossfade" (1) ]
         , U "Convolution3" [ AR, KR ] [ I "in" (0), I "kernel" (0), I "trigger" (0), I "framesize" (0) ]
         , U "Crackle" [ AR, KR ] [ I "chaosParam" (1.5) ]
         , U "CuspL" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (1.9), I "xi" (0) ]
         , U "CuspN" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (1.9), I "xi" (0) ]
         , U "DC" [ AR, KR ] [ I "in" (0) ]
         , U "Dbrown" [  ] [ I "lo" (0), I "hi" (0), I "step" (0), I "length" (inf) ]
         , U "Dbufrd" [  ] [ I "bufnum" (0), I "phase" (0), I "loop" (1) ]
         , U "Decay" [ AR, KR ] [ I "in" (0), I "decayTime" (1) ]
         , U "Decay2" [ AR, KR ] [ I "in" (0), I "attackTime" (0.01), I "decayTime" (1) ]
         , U "DecodeB2" [ AR, KR ] [ I "numChans" (0), I "w" (0), I "x" (0), I "y" (0), I "orientation" (0.5) ]
         , U "DegreeToKey" [ AR, KR ] [ I "bufnum" (0), I "in" (0), I "octave" (12) ]
         , U "Delay1" [ AR, KR ] [ I "in" (0) ]
         , U "Delay2" [ AR, KR ] [ I "in" (0) ]
         , U "DelayC" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2) ]
         , U "DelayL" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2) ]
         , U "DelayN" [ AR, KR ] [ I "in" (0), I "maxdelaytime" (0.2), I "delaytime" (0.2) ]
         , U "Demand" [ AR, KR ] [ I "trig" (0), I "reset" (0), I "demandUGens" (0) ]
         , U "DemandEnvGen" [ AR, KR ] [ I "level" (0), I "dur" (0), I "shape" (1), I "curve" (0), I "gate" (1), I "reset" (1), I "levelScale" (1), I "levelBias" (0), I "timeScale" (1), I "doneAction" (0) ]
         , U "DetectSilence" [ AR, KR ] [ I "in" (0), I "amp" (0.0001), I "time" (0.1), I "doneAction" (0) ]
         , U "Dgeom" [  ] [ I "start" (1), I "grow" (2), I "length" (100) ]
         , U "Dibrown" [  ] [ I "lo" (0), I "hi" (0), I "step" (0), I "length" (inf) ]
         , U "DiskIn" [ AR ] [ I "numChannels" (0), I "bufnum" (0) ]
         , U "DiskOut" [ AR ] [ I "bufnum" (0), I "channelsArray" (0) ]
         , U "Diwhite" [  ] [ I "lo" (0), I "hi" (0), I "length" (inf) ]
         , U "Done" [ KR ] [ I "src" (0) ]
         , U "Drand" [  ] [ I "list" (0), I "repeats" (1) ]
         , U "Dseq" [  ] [ I "list" (0), I "repeats" (1) ]
         , U "Dser" [  ] [ I "list" (0), I "repeats" (1) ]
         , U "Dseries" [  ] [ I "start" (1), I "step" (1), I "length" (100) ]
         , U "Dswitch1" [  ] [ I "list" (0), I "index" (0) ]
         , U "Dust" [ AR, KR ] [ I "density" (0) ]
         , U "Dust2" [ AR, KR ] [ I "density" (0) ]
         , U "Duty" [ AR, KR ] [ I "dur" (1), I "reset" (0), I "level" (1), I "doneAction" (0) ]
         , U "Dwhite" [  ] [ I "lo" (0), I "hi" (0), I "length" (inf) ]
         , U "Dxrand" [  ] [ I "list" (0), I "repeats" (1) ]
         , U "DynKlank" [ AR ] [ I "specificationsArrayRef" (0), I "input" (0), I "freqscale" (1), I "freqoffset" (0), I "decayscale" (1) ]
         , U "EnvGen" [ AR, KR ] [ I "envelope" (0), I "gate" (1), I "levelScale" (1), I "levelBias" (0), I "timeScale" (1), I "doneAction" (0) ]
         , U "ExpRand" [  ] [ I "lo" (0.01), I "hi" (1) ]
         , U "FBSineC" [ AR ] [ I "freq" (22050), I "im" (1), I "fb" (0.1), I "a" (1.1), I "c" (0.5), I "xi" (0.1), I "yi" (0.1) ]
         , U "FBSineL" [ AR ] [ I "freq" (22050), I "im" (1), I "fb" (0.1), I "a" (1.1), I "c" (0.5), I "xi" (0.1), I "yi" (0.1) ]
         , U "FBSineN" [ AR ] [ I "freq" (22050), I "im" (1), I "fb" (0.1), I "a" (1.1), I "c" (0.5), I "xi" (0.1), I "yi" (0.1) ]
         , U "FFT" [  ] [ I "buffer" (0), I "in" (0) ]
         , U "FOS" [ AR, KR ] [ I "in" (0), I "a0" (0), I "a1" (0), I "b1" (0) ]
         , U "FSinOsc" [ AR, KR ] [ I "freq" (440), I "iphase" (0) ]
         , U "Filter" [  ] [ I "maxSize" (0) ]
         , U "Fold" [ AR, KR ] [ I "in" (0), I "lo" (0), I "hi" (1) ]
         , U "Formant" [ AR ] [ I "fundfreq" (440), I "formfreq" (1760), I "bwfreq" (880) ]
         , U "Formlet" [ AR, KR ] [ I "in" (0), I "freq" (440), I "attacktime" (1), I "decaytime" (1) ]
         , U "Free" [ KR ] [ I "trig" (0), I "id" (0) ]
         , U "FreeSelf" [ KR ] [ I "in" (0) ]
         , U "FreeSelfWhenDone" [ KR ] [ I "src" (0) ]
         , U "Gate" [ AR, KR ] [ I "in" (0), I "trig" (0) ]
         , U "GbmanL" [ AR ] [ I "freq" (22050), I "xi" (1.2), I "yi" (2.1) ]
         , U "GbmanN" [ AR ] [ I "freq" (22050), I "xi" (1.2), I "yi" (2.1) ]
         , U "Gendy1" [ AR, KR ] [ I "ampdist" (1), I "durdist" (1), I "adparam" (1), I "ddparam" (1), I "minfreq" (440), I "maxfreq" (660), I "ampscale" (0.5), I "durscale" (0.5), I "initCPs" (12), I "knum" (0) ]
         , U "Gendy2" [ AR, KR ] [ I "ampdist" (1), I "durdist" (1), I "adparam" (1), I "ddparam" (1), I "minfreq" (440), I "maxfreq" (660), I "ampscale" (0.5), I "durscale" (0.5), I "initCPs" (12), I "knum" (0), I "a" (1.17), I "c" (0.31) ]
         , U "Gendy3" [ AR, KR ] [ I "ampdist" (1), I "durdist" (1), I "adparam" (1), I "ddparam" (1), I "freq" (440), I "ampscale" (0.5), I "durscale" (0.5), I "initCPs" (12), I "knum" (0) ]
         , U "GrayNoise" [ AR, KR ] [  ]
         , U "HPF" [ AR, KR ] [ I "in" (0), I "freq" (440) ]
         , U "HPZ1" [ AR, KR ] [ I "in" (0) ]
         , U "HPZ2" [ AR, KR ] [ I "in" (0) ]
         , U "Hasher" [ AR, KR ] [ I "in" (0) ]
         , U "HenonC" [ AR ] [ I "freq" (22050), I "a" (1.4), I "b" (0.3), I "x0" (0), I "x1" (0) ]
         , U "HenonL" [ AR ] [ I "freq" (22050), I "a" (1.4), I "b" (0.3), I "x0" (0), I "x1" (0) ]
         , U "HenonN" [ AR ] [ I "freq" (22050), I "a" (1.4), I "b" (0.3), I "x0" (0), I "x1" (0) ]
         , U "Hilbert" [ AR ] [ I "in" (0) ]
         , U "HilbertFIR" [ AR ] [ I "in" (0), I "buffer" (0) ]
         , U "IFFT" [  ] [ I "buffer" (0) ]
         , U "IRand" [  ] [ I "lo" (0), I "hi" (127) ]
         , U "ImageWarp" [ AR, KR ] [ I "pic" (0), I "x" (0), I "y" (0), I "interpolationType" (1) ]
         , U "Impulse" [ AR, KR ] [ I "freq" (440), I "phase" (0) ]
         , U "In" [ AR, KR ] [ I "bus" (0), I "numChannels" (1) ]
         , U "InFeedback" [ AR ] [ I "bus" (0), I "numChannels" (1) ]
         , U "InRange" [ AR, KR ] [ I "in" (0), I "lo" (0), I "hi" (1) ]
         , U "InRect" [ AR, KR ] [ I "x" (0), I "y" (0), I "rect" (0) ]
         , U "InTrig" [ KR ] [ I "bus" (0), I "numChannels" (1) ]
         , U "Index" [ AR, KR ] [ I "bufnum" (0), I "in" (0) ]
         , U "InfoUGenBase" [ IR ] [  ]
         , U "Integrator" [ AR, KR ] [ I "in" (0), I "coef" (1) ]
         , U "K2A" [ AR ] [ I "in" (0) ]
         , U "KeyState" [ KR ] [ I "keycode" (0), I "minval" (0), I "maxval" (1), I "lag" (0.2) ]
         , U "Klang" [ AR ] [ I "specificationsArrayRef" (0), I "freqscale" (1), I "freqoffset" (0) ]
         , U "Klank" [ AR ] [ I "specificationsArrayRef" (0), I "input" (0), I "freqscale" (1), I "freqoffset" (0), I "decayscale" (1) ]
         , U "LFClipNoise" [ AR, KR ] [ I "freq" (500) ]
         , U "LFCub" [ AR, KR ] [ I "freq" (440), I "iphase" (0) ]
         , U "LFDClipNoise" [ AR, KR ] [ I "freq" (500) ]
         , U "LFDNoise0" [ AR, KR ] [ I "freq" (500) ]
         , U "LFDNoise1" [ AR, KR ] [ I "freq" (500) ]
         , U "LFDNoise3" [ AR, KR ] [ I "freq" (500) ]
         , U "LFNoise0" [ AR, KR ] [ I "freq" (500) ]
         , U "LFNoise1" [ AR, KR ] [ I "freq" (500) ]
         , U "LFNoise2" [ AR, KR ] [ I "freq" (500) ]
         , U "LFPar" [ AR, KR ] [ I "freq" (440), I "iphase" (0) ]
         , U "LFPulse" [ AR, KR ] [ I "freq" (440), I "iphase" (0), I "width" (0.5) ]
         , U "LFSaw" [ AR, KR ] [ I "freq" (440), I "iphase" (0) ]
         , U "LFTri" [ AR, KR ] [ I "freq" (440), I "iphase" (0) ]
         , U "LPF" [ AR, KR ] [ I "in" (0), I "freq" (440) ]
         , U "LPZ1" [ AR, KR ] [ I "in" (0) ]
         , U "LPZ2" [ AR, KR ] [ I "in" (0) ]
         , U "Lag" [ AR, KR ] [ I "in" (0), I "lagTime" (0.1) ]
         , U "Lag2" [ AR, KR ] [ I "in" (0), I "lagTime" (0.1) ]
         , U "Lag3" [ AR, KR ] [ I "in" (0), I "lagTime" (0.1) ]
         , U "LagControl" [ KR, IR ] [ I "values" (0), I "lags" (0) ]
         , U "LagIn" [ KR ] [ I "bus" (0), I "numChannels" (1), I "lag" (0.1) ]
         , U "LastValue" [ AR, KR ] [ I "in" (0), I "diff" (0.01) ]
         , U "Latch" [ AR, KR ] [ I "in" (0), I "trig" (0) ]
         , U "Latoocarfian" [ AR, KR ] [ I "a" (0), I "b" (0), I "c" (0), I "d" (0) ]
         , U "LatoocarfianC" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (3), I "c" (0.5), I "d" (0.5), I "xi" (0.5), I "yi" (0.5) ]
         , U "LatoocarfianL" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (3), I "c" (0.5), I "d" (0.5), I "xi" (0.5), I "yi" (0.5) ]
         , U "LatoocarfianN" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (3), I "c" (0.5), I "d" (0.5), I "xi" (0.5), I "yi" (0.5) ]
         , U "LeakDC" [ AR, KR ] [ I "in" (0), I "coef" (0.995) ]
         , U "LeastChange" [ AR, KR ] [ I "a" (0), I "b" (0) ]
         , U "Limiter" [ AR ] [ I "in" (0), I "level" (1), I "dur" (0.01) ]
         , U "LinCongC" [ AR ] [ I "freq" (22050), I "a" (1.1), I "c" (0.13), I "m" (1), I "xi" (0) ]
         , U "LinCongL" [ AR ] [ I "freq" (22050), I "a" (1.1), I "c" (0.13), I "m" (1), I "xi" (0) ]
         , U "LinCongN" [ AR ] [ I "freq" (22050), I "a" (1.1), I "c" (0.13), I "m" (1), I "xi" (0) ]
         , U "LinExp" [ AR, KR ] [ I "in" (0), I "srclo" (0), I "srchi" (1), I "dstlo" (1), I "dsthi" (2) ]
         , U "LinLin" [ AR, KR ] [ I "in" (0), I "srclo" (0), I "srchi" (1), I "dstlo" (1), I "dsthi" (2) ]
         , U "LinPan2" [ AR, KR ] [ I "in" (0), I "pos" (0), I "level" (1) ]
         , U "LinRand" [  ] [ I "lo" (0), I "hi" (1), I "minmax" (0) ]
         , U "LinXFade2" [ AR, KR ] [ I "inA" (0), I "inB" (0), I "pan" (0), I "level" (1) ]
         , U "Line" [ AR, KR ] [ I "start" (0), I "end" (1), I "dur" (1), I "doneAction" (0) ]
         , U "Linen" [ KR ] [ I "gate" (1), I "attackTime" (0.01), I "susLevel" (1), I "releaseTime" (1), I "doneAction" (0) ]
         , U "ListDUGen" [  ] [ I "list" (0), I "repeats" (1) ]
         , U "LocalIn" [ AR, KR ] [ I "numChannels" (1) ]
         , U "LocalOut" [ AR, KR ] [ I "channelsArray" (0) ]
         , U "Logistic" [ AR, KR ] [ I "chaosParam" (3), I "freq" (1000) ]
         , U "LorenzL" [ AR ] [ I "freq" (22050), I "s" (10), I "r" (28), I "b" (2.667), I "h" (0.05), I "xi" (0.1), I "yi" (0), I "zi" (0) ]
         , U "MantissaMask" [ AR, KR ] [ I "in" (0), I "bits" (3) ]
         , U "Median" [ AR, KR ] [ I "length" (3), I "in" (0) ]
         , U "MidEQ" [ AR, KR ] [ I "in" (0), I "freq" (440), I "rq" (1), I "db" (0) ]
         , U "MostChange" [ AR, KR ] [ I "a" (0), I "b" (0) ]
         , U "MouseButton" [ KR ] [ I "minval" (0), I "maxval" (1), I "lag" (0.2) ]
         , U "MouseX" [ KR ] [ I "minval" (0), I "maxval" (1), I "warp" (0), I "lag" (0.2) ]
         , U "MouseY" [ KR ] [ I "minval" (0), I "maxval" (1), I "warp" (0), I "lag" (0.2) ]
         , U "MulAdd" [  ] [ I "in" (0) ]
         , U "MultiOutUGen" [  ] [ I "maxSize" (0) ]
         , U "NRand" [  ] [ I "lo" (0), I "hi" (1), I "n" (0) ]
         , U "NoahNoise" [ AR, KR ] [  ]
         , U "Normalizer" [ AR ] [ I "in" (0), I "level" (1), I "dur" (0.01) ]
         , U "NumAudioBuses" [ IR ] [  ]
         , U "NumBuffers" [ IR ] [  ]
         , U "NumControlBuses" [ IR ] [  ]
         , U "NumInputBuses" [ IR ] [  ]
         , U "NumOutputBuses" [ IR ] [  ]
         , U "NumRunningSynths" [ IR ] [  ]
         , U "OffsetOut" [ AR, KR ] [ I "bus" (0), I "channelsArray" (0) ]
         , U "OnePole" [ AR, KR ] [ I "in" (0), I "coef" (0.5) ]
         , U "OneZero" [ AR, KR ] [ I "in" (0), I "coef" (0.5) ]
         , U "Osc" [ AR, KR ] [ I "bufnum" (0), I "freq" (440), I "phase" (0) ]
         , U "OscN" [ AR, KR ] [ I "bufnum" (0), I "freq" (440), I "phase" (0) ]
         , U "Out" [ AR, KR ] [ I "bus" (0), I "channelsArray" (0) ]
         , U "OutputProxy" [  ] [ I "rate" (0), I "itsSourceUGen" (0), I "index" (0) ]
         , U "PSinGrain" [ AR ] [ I "freq" (440), I "dur" (0.2), I "amp" (1) ]
         , U "PV_Add" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_BinScramble" [  ] [ I "buffer" (0), I "wipe" (0), I "width" (0.2), I "trig" (0) ]
         , U "PV_BinShift" [  ] [ I "buffer" (0), I "stretch" (1), I "shift" (0) ]
         , U "PV_BinWipe" [  ] [ I "bufferA" (0), I "bufferB" (0), I "wipe" (0) ]
         , U "PV_BrickWall" [  ] [ I "buffer" (0), I "wipe" (0) ]
         , U "PV_ConformalMap" [  ] [ I "buffer" (0), I "areal" (0), I "aimag" (0) ]
         , U "PV_Copy" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_CopyPhase" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_Diffuser" [  ] [ I "buffer" (0), I "trig" (0) ]
         , U "PV_HainsworthFoote" [ AR ] [ I "buffer" (0), I "proph" (0), I "propf" (0), I "threshold" (1), I "waittime" (0.04) ]
         , U "PV_JensenAndersen" [ AR ] [ I "buffer" (0), I "propsc" (0.25), I "prophfe" (0.25), I "prophfc" (0.25), I "propsf" (0.25), I "threshold" (1), I "waittime" (0.04) ]
         , U "PV_LocalMax" [  ] [ I "buffer" (0), I "threshold" (0) ]
         , U "PV_MagAbove" [  ] [ I "buffer" (0), I "threshold" (0) ]
         , U "PV_MagBelow" [  ] [ I "buffer" (0), I "threshold" (0) ]
         , U "PV_MagClip" [  ] [ I "buffer" (0), I "threshold" (0) ]
         , U "PV_MagFreeze" [  ] [ I "buffer" (0), I "freeze" (0) ]
         , U "PV_MagMul" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_MagNoise" [  ] [ I "buffer" (0) ]
         , U "PV_MagShift" [  ] [ I "buffer" (0), I "stretch" (1), I "shift" (0) ]
         , U "PV_MagSmear" [  ] [ I "buffer" (0), I "bins" (0) ]
         , U "PV_MagSquared" [  ] [ I "buffer" (0) ]
         , U "PV_Max" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_Min" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_Mul" [  ] [ I "bufferA" (0), I "bufferB" (0) ]
         , U "PV_PhaseShift" [  ] [ I "buffer" (0), I "shift" (0) ]
         , U "PV_PhaseShift270" [  ] [ I "buffer" (0) ]
         , U "PV_PhaseShift90" [  ] [ I "buffer" (0) ]
         , U "PV_RandComb" [  ] [ I "buffer" (0), I "wipe" (0), I "trig" (0) ]
         , U "PV_RandWipe" [  ] [ I "bufferA" (0), I "bufferB" (0), I "wipe" (0), I "trig" (0) ]
         , U "PV_RectComb" [  ] [ I "buffer" (0), I "numTeeth" (0), I "phase" (0), I "width" (0.5) ]
         , U "PV_RectComb2" [  ] [ I "bufferA" (0), I "bufferB" (0), I "numTeeth" (0), I "phase" (0), I "width" (0.5) ]
         , U "Pan2" [ AR, KR ] [ I "in" (0), I "pos" (0), I "level" (1) ]
         , U "Pan4" [ AR, KR ] [ I "in" (0), I "xpos" (0), I "ypos" (0), I "level" (1) ]
         , U "PanAz" [ AR, KR ] [ I "numChans" (0), I "in" (0), I "pos" (0), I "level" (1), I "width" (2), I "orientation" (0.5) ]
         , U "PanB" [ AR, KR ] [ I "in" (0), I "azimuth" (0), I "elevation" (0), I "gain" (1) ]
         , U "PanB2" [ AR, KR ] [ I "in" (0), I "azimuth" (0), I "gain" (1) ]
         , U "Panner" [  ] [ I "maxSize" (0) ]
         , U "Pause" [ KR ] [ I "gate" (0), I "id" (0) ]
         , U "PauseSelf" [ KR ] [ I "in" (0) ]
         , U "PauseSelfWhenDone" [ KR ] [ I "src" (0) ]
         , U "Peak" [ AR, KR ] [ I "trig" (0), I "reset" (0) ]
         , U "PeakFollower" [ AR, KR ] [ I "in" (0), I "decay" (0.999) ]
         , U "Phasor" [ AR, KR ] [ I "trig" (0), I "rate" (1), I "start" (0), I "end" (1), I "resetPos" (0) ]
         , U "PinkNoise" [ AR, KR ] [  ]
         , U "Pitch" [ KR ] [ I "in" (0), I "initFreq" (440), I "minFreq" (60), I "maxFreq" (4000), I "execFreq" (100), I "maxBinsPerOctave" (16), I "median" (1), I "ampThreshold" (0.01), I "peakThreshold" (0.5), I "downSample" (1) ]
         , U "PitchShift" [ AR ] [ I "in" (0), I "windowSize" (0.2), I "pitchRatio" (1), I "pitchDispersion" (0), I "timeDispersion" (0) ]
         , U "PlayBuf" [ AR ] [ I "numChannels" (0), I "bufnum" (0), I "rate" (1), I "trigger" (1), I "startPos" (0), I "loop" (0) ]
         , U "Poll" [ AR, KR ] [ I "trig" (0), I "in" (0), I "label" (0), I "trigid" (-1) ]
         , U "Pulse" [ AR ] [ I "freq" (440), I "width" (0.5) ]
         , U "PulseCount" [ AR, KR ] [ I "trig" (0), I "reset" (0) ]
         , U "PulseDivider" [ AR, KR ] [ I "trig" (0), I "div" (2), I "start" (0) ]
         , U "QuadC" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (-1), I "c" (-0.75), I "xi" (0) ]
         , U "QuadL" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (-1), I "c" (-0.75), I "xi" (0) ]
         , U "QuadN" [ AR ] [ I "freq" (22050), I "a" (1), I "b" (-1), I "c" (-0.75), I "xi" (0) ]
         , U "RHPF" [ AR, KR ] [ I "in" (0), I "freq" (440), I "rq" (1) ]
         , U "RLPF" [ AR, KR ] [ I "in" (0), I "freq" (440), I "rq" (1) ]
         , U "RadiansPerSample" [ IR ] [  ]
         , U "Ramp" [ AR, KR ] [ I "in" (0), I "lagTime" (0.1) ]
         , U "Rand" [  ] [ I "lo" (0), I "hi" (1) ]
         , U "RandID" [ KR, IR ] [ I "id" (0) ]
         , U "RandSeed" [ KR, IR ] [ I "trig" (0), I "seed" (56789) ]
         , U "RecordBuf" [ AR ] [ I "inputArray" (0), I "bufnum" (0), I "offset" (0), I "recLevel" (1), I "preLevel" (0), I "run" (1), I "loop" (1), I "trigger" (1) ]
         , U "ReplaceOut" [ AR, KR ] [ I "bus" (0), I "channelsArray" (0) ]
         , U "Resonz" [ AR, KR ] [ I "in" (0), I "freq" (440), I "bwr" (1) ]
         , U "Ringz" [ AR, KR ] [ I "in" (0), I "freq" (440), I "decaytime" (1) ]
         , U "Rotate2" [ AR, KR ] [ I "x" (0), I "y" (0), I "pos" (0) ]
         , U "RunningMax" [ AR, KR ] [ I "trig" (0), I "reset" (0) ]
         , U "RunningMin" [ AR, KR ] [ I "trig" (0), I "reset" (0) ]
         , U "RunningSum" [ AR, KR ] [ I "in" (0), I "numsamp" (40) ]
         , U "SOS" [ AR, KR ] [ I "in" (0), I "a0" (0), I "a1" (0), I "a2" (0), I "b1" (0), I "b2" (0) ]
         , U "SampleDur" [ IR ] [  ]
         , U "SampleRate" [ IR ] [  ]
         , U "Saw" [ AR ] [ I "freq" (440) ]
         , U "Schmidt" [ AR, KR ] [ I "in" (0), I "lo" (0), I "hi" (1) ]
         , U "ScopeOut" [ AR, KR ] [ I "inputArray" (0), I "bufnum" (0) ]
         , U "Select" [ AR, KR ] [ I "which" (0), I "array" (0) ]
         , U "SendTrig" [ AR, KR ] [ I "in" (0), I "id" (0), I "value" (0) ]
         , U "SetResetFF" [ AR, KR ] [ I "trig" (0), I "reset" (0) ]
         , U "Shaper" [ AR, KR ] [ I "bufnum" (0), I "in" (0) ]
         , U "SharedIn" [ KR ] [ I "bus" (0), I "numChannels" (1) ]
         , U "SharedOut" [ KR ] [ I "bus" (0), I "channelsArray" (0) ]
         , U "Silent" [ AR ] [ I "numChannels" (1) ]
         , U "SinOsc" [ AR, KR ] [ I "freq" (440), I "phase" (0) ]
         , U "SinOscFB" [ AR, KR ] [ I "freq" (440), I "feedback" (0) ]
         , U "Slew" [ AR, KR ] [ I "in" (0), I "up" (1), I "dn" (1) ]
         , U "Slope" [ AR, KR ] [ I "in" (0) ]
         , U "Spring" [ AR ] [ I "in" (0), I "spring" (1), I "damp" (0) ]
         , U "StandardL" [ AR ] [ I "freq" (22050), I "k" (1), I "xi" (0.5), I "yi" (0) ]
         , U "StandardN" [ AR ] [ I "freq" (22050), I "k" (1), I "xi" (0.5), I "yi" (0) ]
         , U "Stepper" [ AR, KR ] [ I "trig" (0), I "reset" (0), I "min" (0), I "max" (7), I "step" (1), I "resetval" (0) ]
         , U "SubsampleOffset" [ IR ] [  ]
         , U "Sweep" [ AR, KR ] [ I "trig" (0), I "rate" (1) ]
         , U "SyncSaw" [ AR, KR ] [ I "syncFreq" (440), I "sawFreq" (440) ]
         , U "TBall" [ AR ] [ I "in" (0), I "g" (10), I "damp" (0), I "friction" (0.01) ]
         , U "TDelay" [ AR, KR ] [ I "in" (0), I "dur" (0.1) ]
         , U "TDuty" [ AR, KR ] [ I "dur" (1), I "reset" (0), I "level" (1), I "doneAction" (0) ]
         , U "TExpRand" [ AR, KR ] [ I "lo" (0.01), I "hi" (1), I "trig" (0) ]
         , U "TGrains" [ AR ] [ I "numChannels" (0), I "trigger" (0), I "bufnum" (0), I "rate" (1), I "centerPos" (0), I "dur" (0.1), I "pan" (0), I "amp" (0.1), I "interp" (4) ]
         , U "TIRand" [ AR, KR ] [ I "lo" (0), I "hi" (127), I "trig" (0) ]
         , U "TPulse" [ AR, KR ] [ I "trig" (0), I "freq" (440), I "width" (0.5) ]
         , U "TRand" [ AR, KR ] [ I "lo" (0), I "hi" (1), I "trig" (0) ]
         , U "TWChoose" [ AR, KR ] [ I "trig" (0), I "array" (0), I "weights" (0), I "normalize" (0) ]
         , U "TWindex" [ AR, KR ] [ I "in" (0), I "array" (0), I "normalize" (0) ]
         , U "Tap" [ AR ] [ I "bufnum" (0), I "numChannels" (1), I "delaytime" (0.2) ]
         , U "Timer" [ AR, KR ] [ I "trig" (0) ]
         , U "ToggleFF" [ AR, KR ] [ I "trig" (0) ]
         , U "Trapezoid" [ AR, KR ] [ I "in" (0), I "a" (0.2), I "b" (0.4), I "c" (0.6), I "d" (0.8) ]
         , U "Trig" [ AR, KR ] [ I "in" (0), I "dur" (0.1) ]
         , U "Trig1" [ AR, KR ] [ I "in" (0), I "dur" (0.1) ]
         , U "TrigControl" [ KR, IR ] [ I "values" (0) ]
         , U "TwoPole" [ AR, KR ] [ I "in" (0), I "freq" (440), I "radius" (0.8) ]
         , U "TwoZero" [ AR, KR ] [ I "in" (0), I "freq" (440), I "radius" (0.8) ]
         , U "UnaryOpUGen" [  ] [ I "selector" (0), I "a" (0) ]
         , U "VOsc" [ AR, KR ] [ I "bufpos" (0), I "freq" (440), I "phase" (0) ]
         , U "VOsc3" [ AR, KR ] [ I "bufpos" (0), I "freq1" (110), I "freq2" (220), I "freq3" (440) ]
         , U "VarSaw" [ AR, KR ] [ I "freq" (440), I "iphase" (0), I "width" (0.5) ]
         , U "Vibrato" [ AR, KR ] [ I "freq" (440), I "rate" (6), I "depth" (0.02), I "delay" (0), I "onset" (0), I "rateVariation" (0.04), I "depthVariation" (0.1), I "iphase" (0) ]
         , U "WhiteNoise" [ AR, KR ] [  ]
         , U "Wrap" [ AR, KR ] [ I "in" (0), I "lo" (0), I "hi" (1) ]
         , U "WrapIndex" [ AR, KR ] [ I "bufnum" (0), I "in" (0) ]
         , U "XFade" [  ] [ I "maxSize" (0) ]
         , U "XFade2" [ AR, KR ] [ I "inA" (0), I "inB" (0), I "pan" (0), I "level" (1) ]
         , U "XLine" [ AR, KR ] [ I "start" (1), I "end" (2), I "dur" (1), I "doneAction" (0) ]
         , U "XOut" [ AR, KR ] [ I "bus" (0), I "xfade" (0), I "channelsArray" (0) ]
         , U "XY" [ AR, KR ] [ I "xscale" (1), I "yscale" (1), I "xoff" (0), I "yoff" (0), I "rot" (0), I "rate" (1) ]
         , U "ZeroCrossing" [ AR, KR ] [ I "in" (0) ]
         ]
-- Local Variables:
-- truncate-lines:t
-- End:
