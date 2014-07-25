-- | Enumerations of the unary and binary math unit generators.
module Sound.SC3.UGen.Operator where

import Data.Maybe
import Data.List

-- | Enumeration of @SC3@ unary operator UGens.
data Unary  = Neg
            | Not
            | IsNil
            | NotNil
            | BitNot
            | Abs
            | AsFloat
            | AsInt
            | Ceil
            | Floor
            | Frac
            | Sign
            | Squared
            | Cubed
            | Sqrt
            | Exp
            | Recip
            | MIDICPS
            | CPSMIDI
            | MIDIRatio
            | RatioMIDI
            | DbAmp
            | AmpDb
            | OctCPS
            | CPSOct
            | Log
            | Log2
            | Log10
            | Sin
            | Cos
            | Tan
            | ArcSin
            | ArcCos
            | ArcTan
            | SinH
            | CosH
            | TanH
            | Rand
            | Rand2
            | LinRand
            | BiLinRand
            | Sum3Rand
            | Distort
            | SoftClip
            | Coin
            | DigitValue
            | Silence
            | Thru
            | RectWindow
            | HanWindow
            | WelchWindow
            | TriWindow
            | Ramp
            | SCurve
              deriving (Eq,Show,Enum,Read)

-- | Variant of 'reads' requiring exact match.
reads_exact :: Read a => String -> Maybe a
reads_exact s =
    case reads s of
      [(r,"")] -> Just r
      _ -> Nothing

-- | Type-specialised 'reads_exact'.
parse_unary :: String -> Maybe Unary
parse_unary = reads_exact

-- | Enumeration of @SC3@ unary operator UGens.
data Binary = Add
            | Sub
            | Mul
            | IDiv
            | FDiv
            | Mod
            | EQ_
            | NE
            | LT_
            | GT_
            | LE
            | GE
            | Min
            | Max
            | BitAnd
            | BitOr
            | BitXor
            | LCM
            | GCD
            | Round
            | RoundUp
            | Trunc
            | Atan2
            | Hypot
            | Hypotx
            | Pow
            | ShiftLeft
            | ShiftRight
            | UnsignedShift
            | Fill
            | Ring1
            | Ring2
            | Ring3
            | Ring4
            | DifSqr
            | SumSqr
            | SqrSum
            | SqrDif
            | AbsDif
            | Thresh
            | AMClip
            | ScaleNeg
            | Clip2
            | Excess
            | Fold2
            | Wrap2
            | FirstArg
            | RandRange
            | ExpRandRange
              deriving (Eq,Show,Enum,Read)

-- | Type-specialised 'reads_exact'.
parse_binary :: String -> Maybe Binary
parse_binary = reads_exact

-- | Table of symbolic names for standard unary operators.
unaryTable :: [(Int,String)]
unaryTable = [(0,"-")]

-- | Lookup possibly symbolic name for standard unary operators.
unaryName :: Int -> String
unaryName n =
    let s = show (toEnum n :: Unary)
    in fromMaybe s (lookup n unaryTable)

-- | Table of symbolic names for standard binary operators.
binaryTable :: [(Int,String)]
binaryTable =
    [(0,"+")
    ,(1,"-")
    ,(2,"*")
    ,(4,"/")
    ,(5,"%")
    ,(6,"==")
    ,(7,"/=")
    ,(8,"<")
    ,(9,">")
    ,(10,"<=")
    ,(11,">=")
    ,(25,"**")]

-- | Lookup possibly symbolic name for standard binary operators.
--
-- > map binaryName [1,2,8] == ["-","*","<"]
binaryName :: Int -> String
binaryName n =
    let s = show (toEnum n :: Binary)
    in fromMaybe s (lookup n binaryTable)

-- | Reverse 'lookup'.
rlookup :: Eq b => b -> [(a,b)] -> Maybe a
rlookup x = fmap fst . find ((== x) . snd)

-- | Given name of binary operator derive index.
--
-- > mapMaybe binaryIndex ["*","Mul","Ring1"] == [2,2,30]
-- > binaryIndex "SinOsc" == Nothing
binaryIndex :: String -> Maybe Int
binaryIndex nm = maybe (fmap fromEnum (parse_binary nm)) Just (rlookup nm binaryTable)

-- | Given name of unary operator derive index.
--
-- > mapMaybe unaryIndex ["-","Neg","Cubed"] == [0,0,13]
-- > unaryIndex "SinOsc" == Nothing
unaryIndex :: String -> Maybe Int
unaryIndex nm = maybe (fmap fromEnum (parse_unary nm)) Just (rlookup nm unaryTable)
