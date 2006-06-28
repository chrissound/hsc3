module Hsc.Math where

import Hsc.UGen (UGen(Constant, MCE), Special)
import Hsc.Construct (mkFilter)

--binop :: Special -> UGen -> UGen -> UGen
binop _ f (Constant a) (Constant b) = Constant (f a b)
binop i _ a            b = mkFilter "BinaryOpUGen" [a,b] 1 i

uop :: Special -> UGen -> UGen
uop   i a   = mkFilter "UnaryOpUGen"  [a]   1 i

instance Num UGen where
    negate         = uop 0
    (+)            = binop 0 (+)
    (-)            = binop 1 (-)
    (*)            = binop 2 (*)
    abs            = uop 5
    signum         = uop 11
    fromInteger a  = Constant (fromInteger a)

instance Fractional UGen where
    recip          = uop 16
    (/)            = binop 4 (/)
    fromRational a = Constant (fromRational a)

instance Floating UGen where
    pi             = Constant pi
    exp            = uop 15
    log            = uop 25
    sqrt           = uop 14
    (**)           = binop 25 (**)
    logBase a b    = log b / log a
    sin            = uop 28
    cos            = uop 29
    tan            = uop 30
    asin           = uop 31
    acos           = uop 32
    atan           = uop 33
    sinh           = uop 34
    cosh           = uop 35
    tanh           = uop 36
    -- cf. module Haskore.Interface.CSound.Orchestra
    asinh x        = log (sqrt (x*x+1) + x)
    acosh x        = log (sqrt (x*x-1) + x)
    atanh x        = (log (1+x) - log (1-x)) / 2

-- The Eq and Ord classes in the Prelude require Bool, hence the name
-- mangling.  True is 1.0, False is 0.0

instance Ord UGen where
    (<)  = error "Ord is partial, see OrdE"
    (<=) = error "Ord is partial, see OrdE"
    (>)  = error "Ord is partial, see OrdE"
    (>=) = error "Ord is partial, see OrdE"
    min  = binop 12 min
    max  = binop 13 max

class EqE a where
    (==*)  :: a -> a -> a
    (/=*)  :: a -> a -> a

instance EqE UGen where
    (==*)  = binop 6 (==*)
    (/=*)  = binop 7 (/=*)

instance EqE Float where
    a ==* b = if a == b then 1.0 else 0.0
    a /=* b = if a /= b then 1.0 else 0.0

class OrdE a where
    (<*)  :: a -> a -> a
    (<=*) :: a -> a -> a
    (>*)  :: a -> a -> a
    (>=*) :: a -> a -> a

instance OrdE UGen where
    (<*)  = binop 8  (<*)
    (<=*) = binop 10 (<=*)
    (>*)  = binop 9  (>*)
    (>=*) = binop 11 (>=*)

instance OrdE Float where
    a <* b   = if a < b   then 1.0 else 0.0
    a <=* b  = if a <= b  then 1.0 else 0.0
    a >* b   = if a > b   then 1.0 else 0.0
    a >=* b  = if a >= b  then 1.0 else 0.0

class (Num a, Fractional a, Floating a) => UnaryOp a where
    notE           :: a -> a
    isnil          :: a -> a
    notnil         :: a -> a
    bitnot         :: a -> a
    asfloat        :: a -> a
    asint          :: a -> a
    ceil           :: a -> a
    floorE         :: a -> a
    frac           :: a -> a
    squared        :: a -> a
    cubed          :: a -> a
    midicps        :: a -> a
    cpsmidi        :: a -> a
    midiratio      :: a -> a
    ratiomidi      :: a -> a
    dbamp          :: a -> a
    ampdb          :: a -> a
    octcps         :: a -> a
    cpsoct         :: a -> a
    log2           :: a -> a
    log10          :: a -> a
    softclip       :: a -> a

instance UnaryOp UGen where
    notE           = uop 1
    isnil          = uop 2
    notnil         = uop 3
    bitnot         = uop 4
    asfloat        = uop 6
    asint          = uop 7
    ceil           = uop 8
    floorE         = uop 9
    frac           = uop 10
    squared        = uop 12
    cubed          = uop 13
    midicps        = uop 17
    cpsmidi        = uop 18
    midiratio      = uop 19
    ratiomidi      = uop 20
    dbamp          = uop 21
    ampdb          = uop 22
    octcps         = uop 23
    cpsoct         = uop 24
    log2           = uop 26
    log10          = uop 27
    softclip       = uop 43

instance UnaryOp Float where
    notE a      = if a >  0.0 then 0.0 else 1.0
    isnil a     = if a == 0.0 then 0.0 else 1.0
    notnil a    = if a /= 0.0 then 0.0 else 1.0
    bitnot a    = a
    asfloat a   = a
    asint a     = a
    ceil a      = fromIntegral (ceiling a)
    floorE a    = fromIntegral (floor a)
    frac a      = a
    squared a   = a * a
    cubed   a   = a * a * a
    midicps a   = 440.0 * (2.0 ** ((a - 69.0) * 0.083333333333))
    cpsmidi a   = (log2 (a * 0.0022727272727) * 12.0) + 69.0
    midiratio a = 2.0 ** (a * 0.083333333333)
    ratiomidi a = 12.0 * (log2 a)
    dbamp a     = a
    ampdb a     = a
    octcps a    = 440.0 * (2.0 ** (a - 4.75))
    cpsoct a    = log2 (a * 0.0022727272727) + 4.75
    log2 a      = logBase 2 a
    log10 a     = logBase 10 a
    softclip a  = a

class (Num a, Fractional a, Floating a) => BinaryOp a where
    idiv           :: a -> a -> a
    mod            :: a -> a -> a
    bitand         :: a -> a -> a
    bitor          :: a -> a -> a
    bitxor         :: a -> a -> a
    lcm            :: a -> a -> a
    gcd            :: a -> a -> a
    round          :: a -> a -> a
    roundup        :: a -> a -> a
    trunc          :: a -> a -> a
    atan2          :: a -> a -> a
    hypot          :: a -> a -> a
    hypotx         :: a -> a -> a
    pow            :: a -> a -> a
    shiftleft      :: a -> a -> a
    shiftright     :: a -> a -> a
    unsignedshift  :: a -> a -> a
    fill           :: a -> a -> a
    ring1          :: a -> a -> a
    ring2          :: a -> a -> a
    ring3          :: a -> a -> a
    ring4          :: a -> a -> a
    difsqr         :: a -> a -> a
    sumsqr         :: a -> a -> a
    sqrdif         :: a -> a -> a
    sqrsum         :: a -> a -> a
    absdif         :: a -> a -> a
    thresh         :: a -> a -> a
    amclip         :: a -> a -> a
    scaleneg       :: a -> a -> a
    clip2          :: a -> a -> a
    excess         :: a -> a -> a
    fold2          :: a -> a -> a
    wrap2          :: a -> a -> a
    firstarg       :: a -> a -> a
    randrange      :: a -> a -> a
    exprandrange   :: a -> a -> a

instance BinaryOp UGen where
    idiv           = binop  3 unimplemented_binop
    mod            = binop  5 unimplemented_binop
    bitand         = binop 14 unimplemented_binop
    bitor          = binop 15 unimplemented_binop
    bitxor         = binop 16 unimplemented_binop
    lcm            = binop 17 unimplemented_binop
    gcd            = binop 18 unimplemented_binop
    round          = binop 19 unimplemented_binop
    roundup        = binop 20 unimplemented_binop
    trunc          = binop 21 unimplemented_binop
    atan2          = binop 22 unimplemented_binop
    hypot          = binop 23 unimplemented_binop
    hypotx         = binop 24 unimplemented_binop
    pow            = binop 25 unimplemented_binop
    shiftleft      = binop 26 unimplemented_binop
    shiftright     = binop 27 unimplemented_binop
    unsignedshift  = binop 28 unimplemented_binop
    fill           = binop 29 unimplemented_binop
    ring1          = binop 30 unimplemented_binop
    ring2          = binop 31 unimplemented_binop
    ring3          = binop 32 unimplemented_binop
    ring4          = binop 33 unimplemented_binop
    difsqr         = binop 34 unimplemented_binop
    sumsqr         = binop 35 unimplemented_binop
    sqrsum         = binop 36 unimplemented_binop
    sqrdif         = binop 37 unimplemented_binop
    absdif         = binop 38 unimplemented_binop
    thresh         = binop 39 unimplemented_binop
    amclip         = binop 40 unimplemented_binop
    scaleneg       = binop 41 unimplemented_binop
    clip2          = binop 42 unimplemented_binop
    excess         = binop 43 unimplemented_binop
    fold2          = binop 44 unimplemented_binop
    wrap2          = binop 45 unimplemented_binop
    firstarg       = binop 46 unimplemented_binop
    randrange      = binop 47 unimplemented_binop
    exprandrange   = binop 48 unimplemented_binop

unimplemented_binop a b = error ("unimplemented binop" ++ show (a,b))

instance BinaryOp Float where
    idiv a b           = unimplemented_binop a b
    mod a b            = unimplemented_binop a b
    bitand a b         = unimplemented_binop a b
    bitor a b          = unimplemented_binop a b
    bitxor a b         = unimplemented_binop a b
    lcm a b            = unimplemented_binop a b
    gcd a b            = unimplemented_binop a b
    round a b          = if b == 0 then a else floorE (a/b + 0.5) * b
    roundup a b        = if b == 0 then a else ceil (a/b + 0.5) * b
    trunc a b          = unimplemented_binop a b
    atan2 a b          = atan (b/a)
    hypot a b          = unimplemented_binop a b
    hypotx a b         = unimplemented_binop a b
    pow a b            = unimplemented_binop a b
    shiftleft a b      = unimplemented_binop a b
    shiftright a b     = unimplemented_binop a b
    unsignedshift a b  = unimplemented_binop a b
    fill a b           = unimplemented_binop a b
    ring1 a b          = a * b + a
    ring2 a b          = a * b + a + b
    ring3 a b          = a * a * b
    ring4 a b          = a * a * b - a * b * b
    difsqr a b         = (a*a) - (b*b)
    sumsqr a b         = (a*a) + (b*b)
    sqrsum a b         = (a+b) * (a+b)
    sqrdif a b         = (a-b) * (a-b)
    absdif a b         = abs (a - b)
    thresh a b         = if a <  b then 0 else a
    amclip a b         = if b <= 0 then 0 else a * b
    scaleneg a b       = (abs a - a) * b' + a where b' = 0.5 * b + 0.5
    clip2 a b          = clip a (-b) b
    excess a b         = a - clip a (-b) b
    fold2 a b          = fold a (-b) b
    wrap2 a b          = wrap a (-b) b
    firstarg a _       = a
    randrange a b      = unimplemented_binop a b
    exprandrange a b   = unimplemented_binop a b

class (Num a, Fractional a, Floating a) => TernaryOp a where
    wrap :: a -> a -> a -> a
    fold :: a -> a -> a -> a
    clip :: a -> a -> a -> a

instance TernaryOp Float where
    wrap a b c = if a >= b && a <= c 
                 then a 
                 else a - r * floorE (a-b)/r 
        where r = c - b
    fold a b c = if a >= b && a <= c 
                 then a 
                 else y' + b
        where r  = c - b
              r' = r + r
              x  = a - b
              y  = x - r' * floorE x/r'
              y' = if y >= r then r' - y else y
    clip a b c = if a < b then b else if a > c then c else a

unimplemented_ternaryop a b c = error ("unimplemented ternary op" ++ show (a, b, c))

instance TernaryOp UGen where
    wrap a b c = unimplemented_ternaryop a b c
    fold a b c = unimplemented_ternaryop a b c
    clip i l h = mkFilter "Clip" [i,l,h] 1 0

--

mix (MCE u)  = foldl1 (+) u
mix u        = u

mix_fill n f = mix (MCE (map f [0..n-1]))
