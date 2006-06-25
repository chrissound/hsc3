module Hsc.U8v where

import Data.Int (Int32, Int64)
import Data.Word (Word8)
import Data.Bits (Bits, shiftL, shiftR, (.&.))
import Data.Char (chr, ord)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (MArray, newArray, readArray, castSTUArray)
import System.IO (openFile, IOMode(..), hPutStr, hClose)

type Float32 = Float
type Float64 = Double

type U8   = Word8
type U8v  = [U8]

asU8 :: (Integral a) => a -> U8
asU8 n = fromIntegral n

byte :: (Integral a, Bits a) => Int -> a -> U8
byte n i = asU8 $ (shiftR i (n * 8)) .&. 0xFF

i8_u8v :: Int -> U8v
i8_u8v n = [asU8 n]

i16_u8v :: Int -> U8v
i16_u8v n = [byte 1 n, byte 0 n]

i32_u8v :: Int -> U8v
i32_u8v n = [byte 3 n, byte 2 n, byte 1 n, byte 0 n]

i64_u8v :: Integer -> U8v
i64_u8v n = [byte 7 n, byte 6 n, byte 5 n, byte 4 n,
             byte 3 n, byte 2 n, byte 1 n, byte 0 n]

u64_u8v = i64_u8v

f32_i32 :: Float32 -> Int32
f32_i32 d = runST ((do a  <- newArray (1, 1) d
                       a' <- castSTUArray a
                       readArray a' 1) :: ST s Int32)

f32_u8v :: Float32 -> U8v
f32_u8v f = i32_u8v (fromIntegral $ f32_i32 f)

f64_i64 :: Float64 -> Int64
f64_i64 d = runST ((do a  <- newArray (1, 1) d
                       a' <- castSTUArray a
                       readArray a' 1) :: ST s Int64)

f64_u8v :: Float64 -> U8v
f64_u8v f = i64_u8v (fromIntegral $ f64_i64 f)

str_u8v :: String -> U8v
str_u8v s = map (asU8 . ord) s

-- pstr = pascal string

pstr_u8v :: String -> U8v
pstr_u8v s = i8_u8v n ++ str_u8v s
    where n = length s

u8v_str :: [U8] -> String
u8v_str s = map (chr . fromIntegral) s

shiftL' n i = shiftL (fromIntegral n) i

u8v_i8 :: U8v -> Int
u8v_i8 [a] = shiftL' a 0

u8v_i16 :: U8v -> Int
u8v_i16 [b,a] = shiftL' b 8 + shiftL' a 0

u8v_i32 :: U8v -> Int
u8v_i32 [d,c,b,a] = shiftL' d 24 + shiftL' c 16 + shiftL' b 8 + shiftL' a 0

u8v_i64 :: U8v -> Integer
u8v_i64 [h,g,f,e,d,c,b,a] = shiftL' h 56 + shiftL' g 48 +
                            shiftL' f 40 + shiftL' e 32 +
                            shiftL' d 24 + shiftL' c 16 +
                            shiftL' b 8 + shiftL' a 0

i32_f32 :: Int32 -> Float32
i32_f32 d = runST ((do a  <- newArray (1, 1) d
                       a' <- castSTUArray a
                       readArray a' 1) :: ST s Float32)

u8v_f32 :: U8v -> Float32
u8v_f32 b = i32_f32 (fromIntegral $ u8v_i32 b)

i64_f64 :: Int64 -> Float64
i64_f64 d = runST ((do a  <- newArray (1, 1) d
                       a' <- castSTUArray a
                       readArray a' 1) :: ST s Float64)

u8v_f64 :: U8v -> Float64
u8v_f64 b = i64_f64 (fromIntegral $ u8v_i64 b)

-- IO

u8vWrite fn u = do h <- openFile fn WriteMode
                   hPutStr h (u8v_str u)
                   hClose h
