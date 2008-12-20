-- | Functions to generate break point data for standard envelope types.
module Sound.SC3.UGen.Envelope.Construct where

import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.Math
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Utilities

-- | Basic envelope data constructor.
env :: [UGen] -> [UGen] -> [EnvCurve] -> UGen -> UGen -> [UGen]
env []     _   _   _   _  = error "env: illegal specification"
env (l:vl) tms crv rls lp =
    [l, n', rls, lp] ++ concat (zipWith3 f vl tms (take n $ cycle crv))
    where f l' t c = [l', t, env_curve c, env_value c]
          n       = length tms
          n'      = fromIntegral n

-- | Co-ordinate based static envelope generator.
envCoord :: [(UGen, UGen)] -> UGen -> UGen -> EnvCurve -> [UGen]
envCoord bp dur amp c = env l t (repeat c) (-1) (-1)
    where l = map ((* amp) . snd) bp
          t = map (* dur) (d_dx (map fst bp))

{- | Trapezoidal envelope generator.  The arguments are: 1. @shape@
determines the sustain time as a proportion of @dur@, zero is a
triangular envelope, one a rectangular envelope; 2. @skew@ determines
the attack\/decay ratio, zero is an immediate attack and a slow decay,
one a slow attack and an immediate decay; 3. @duration@ in seconds;
4. @amplitude@ as linear gain.  -}
envTrapezoid :: UGen -> UGen -> UGen -> UGen -> [UGen]
envTrapezoid shape skew dur amp = envCoord bp dur amp EnvLin
    where x1 = skew * (1 - shape)
          bp = [ (0, skew <=* 0)
               , (x1, 1)
               , (shape + x1, 1)
               , (1, skew >=* 1) ]

envPerc' :: UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
envPerc' atk rls lvl crv = env [0.0, lvl, 0.0] [atk, rls] crv (-1.0) (-1.0)

-- | Percussive envelope, with attack, release, level and curve inputs.
envPerc :: UGen -> UGen -> [UGen]
envPerc atk rls = envPerc' atk rls 1.0 (dbl (EnvNum (-4.0)))

-- | Triangular envelope, with duration and level inputs.
envTriangle :: UGen -> UGen -> [UGen]
envTriangle dur lvl =
   env [0.0, lvl, 0.0] (dbl (dur / 2.0)) (dbl EnvLin) (-1.0) (-1.0)

-- | Sine envelope, with duration and level inputs.
envSine :: UGen -> UGen -> [UGen]
envSine dur lvl =
   env [0.0, lvl, 0.0] (dbl (dur / 2.0)) (dbl EnvSin) (-1.0) (-1.0)

-- | Linear envelope parameter constructor.
envLinen :: UGen -> UGen -> UGen -> UGen -> [EnvCurve] -> [UGen]
envLinen aT sT rT l c = env [0, l, l, 0] [aT, sT, rT] c (-1) (-1)
