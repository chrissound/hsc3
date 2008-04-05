module Sound.SC3.UGen.Demand where

import Sound.SC3.UGen.Rate
import Sound.SC3.UGen.UGen
import Sound.SC3.UGen.UGen.Construct
import Sound.SC3.UGen.UGen.MCE
import Sound.SC3.UGen.Enum
import Sound.SC3.UGen.Utilities

-- | Infinte repeat counter for demand rate unit generators.
dinf :: UGen
dinf = Constant 9E8

-- | Demand results from demand rate ugens.
demand :: UGen -> UGen -> UGen -> UGen
demand t r d = mkFilterKeyed "Demand" 0 (t : r : d') (length d')
    where d' = mceChannels d

-- | Demand envlope generator.
demandEnvGen :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen -> DoneAction -> UGen
demandEnvGen r l d s c g rst ls lb ts a = mkOsc r "DemandEnvGen" [l, d, s, c, g, rst, ls, lb, ts, from_done_action a] 1

-- | Demand results from demand rate ugens.
duty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen
duty rate d r act l = mkOsc rate "Duty" [d, r, from_done_action act, l] 1

-- | Demand results as trigger from demand rate ugens.
tDuty :: Rate -> UGen -> UGen -> DoneAction -> UGen -> UGen -> UGen
tDuty r d rst act l gap = mkOsc r "TDuty" [d, rst, from_done_action act, l, gap] 1

-- Local Variables:
-- truncate-lines:t
-- End:
