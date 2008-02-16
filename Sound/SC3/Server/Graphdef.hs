module Sound.SC3.Server.Graphdef (graphdef) where

import Sound.OpenSoundControl.Byte
import Sound.OpenSoundControl.Cast
import Sound.SC3.UGen.UGen (UGen(..), Special(..))
import Sound.SC3.UGen.Rate (rateId)
import Sound.SC3.UGen.Graph

import Data.Word
import qualified Data.ByteString.Lazy as B

-- | Byte-encode Input value.
encode_input :: Input -> B.ByteString
encode_input (Input u p) = B.append (encode_i16 u) (encode_i16 p)

-- | Byte-encode Control value.
encode_control :: Graph -> UGen -> B.ByteString
encode_control g c@(Control _ n _) = B.concat [ B.pack (str_pstr n)
                                              , encode_i16 (nodeIndex g c)]
encode_control _ _  = error "encode_control: illegal input"

-- | Byte-encode UGen value.
encode_ugen :: Graph -> UGen -> B.ByteString
encode_ugen g (Primitive r n i o s _) = B.concat [ B.pack (str_pstr n)
                                                 , encode_i8 (rateId r)
                                                 , encode_i16 (length i)
                                                 , encode_i16 (length o)
                                                 , encode_i16 s'
                                                 , B.concat i'
                                                 , B.concat o' ]
    where i' = map (encode_input . makeInput g) i
          o' = map (encode_i8 . rateId) o
          (Special s') = s
encode_ugen _ _ = error "encode_ugen: illegal input"

-- | Construct instrument definition bytecode.
encode_graphdef :: String -> Graph -> B.ByteString
encode_graphdef s g = B.concat [ encode_str "SCgf"
                               , encode_i32 0
                               , encode_i16 1
                               , B.pack (str_pstr s)
                               , encode_i16 (length n)
                               , B.concat (map (encode_f32 . constantValue) n)
                               , encode_i16 (length c)
                               , B.concat (map (encode_f32 . controlDefault) c)
                               , encode_i16 (length c)
                               , B.concat (map (encode_control g) c)
                               , encode_i16 (length u)
                               , B.concat (map (encode_ugen g) u) ]
    where (Graph n c u) = g

-- | Construct instrument definition bytecode.
graphdef :: String -> Graph -> [Word8]
graphdef s g = B.unpack (encode_graphdef s g)
