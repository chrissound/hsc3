-- | Data types for enumerated and non signal unit generator inputs.
module Sound.SC3.UGen.Enum where

import Sound.SC3.UGen.UGen

-- | Loop indicator input.
data Loop = Loop
          | NoLoop
          | WithLoop UGen
            deriving (Eq, Show)

-- | Resolve 'Loop'.
from_loop :: Loop -> UGen
from_loop e =
    case e of
      NoLoop -> Constant 0
      Loop -> Constant 1
      WithLoop u -> u

-- | Interpolation indicator input.
data Interpolation = NoInterpolation
                   | LinearInterpolation
                   | CubicInterpolation
                   | Interpolation UGen
                     deriving (Eq, Show)

-- | Resolve 'Interpolation'.
from_interpolation :: Interpolation -> UGen
from_interpolation e =
    case e of
      NoInterpolation -> Constant 1
      LinearInterpolation -> Constant 2
      CubicInterpolation -> Constant 4
      Interpolation u -> u

-- | Completion mode indicator input.
data DoneAction = DoNothing
                | PauseSynth
                | RemoveSynth
                | DoneAction UGen
                  deriving (Eq, Show)

-- | Resolve 'DoneAction'.
from_done_action :: DoneAction -> UGen
from_done_action e =
    case e of
      DoNothing -> Constant 0
      PauseSynth -> Constant 1
      RemoveSynth -> Constant 2
      DoneAction u -> u

-- | Warp interpolation indicator input.
data Warp = Linear
          | Exponential
          | Warp UGen
            deriving (Eq, Show)

-- | Resolve 'Warp'.
from_warp :: Warp -> UGen
from_warp e =
    case e of
      Linear -> Constant 0
      Exponential -> Constant 1
      Warp u -> u

-- | Envelope curve indicator input.
data Envelope_Curve a = EnvStep
                      | EnvLin
                      | EnvExp
                      | EnvSin
                      | EnvCos
                      | EnvNum a
                      | EnvSqr
                      | EnvCub
                        deriving (Eq, Show)

type EnvCurve = Envelope_Curve UGen

-- | Convert 'Envelope_Curve' to shape value.
--
-- > map env_curve_shape [EnvSin,EnvSqr] == [3,6]
env_curve_shape :: Num a => Envelope_Curve a -> a
env_curve_shape e =
    case e of
      EnvStep -> 0
      EnvLin -> 1
      EnvExp -> 2
      EnvSin -> 3
      EnvCos -> 4
      EnvNum _ -> 5
      EnvSqr -> 6
      EnvCub -> 7

-- | The /value/ of 'EnvCurve' is non-zero for 'EnvNum'.
--
-- > map env_curve_value [EnvCos,EnvNum 2] == [0,2]
env_curve_value :: Num a => Envelope_Curve a -> a
env_curve_value e =
    case e of
      EnvNum u -> u
      _ -> 0
