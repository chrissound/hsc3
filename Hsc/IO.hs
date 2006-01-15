module Hsc.IO where

import Hsc.Construct

in' n r bus = mkOsc r "In" [bus] n 0 r0

keystate r key min max lag = mkOsc r "KeyState" [key,min,max,lag] 1 0 r0

mousebutton r min max lag  = mkOsc r "MouseButton"  [min,max,lag] 1 0 r0
mousex r min max warp lag  = mkOsc r "MouseX"  [min,max,warp,lag] 1 0 r0
mousey r min max warp lag  = mkOsc r "MouseY"  [min,max,warp,lag] 1 0 r0

out b i = mkFilterMCE "Out" [b] i 0 0 r0
