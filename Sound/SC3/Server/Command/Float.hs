-- | Specialised to 'Int' and 'Float'.
module Sound.SC3.Server.Command.Float where

import Sound.OSC.Core {- hosc -}

import qualified Sound.SC3.Server.Command.Generic as G
import Sound.SC3.Server.Enum
import Sound.SC3.UGen.Enum

-- | Fill ranges of a node's control values.
n_fill :: Int -> [(String,Int,Float)] -> Message
n_fill = G.n_fill

-- | Set a node's control values.
n_set :: Int -> [(String,Float)] -> Message
n_set = G.n_set

-- | Set ranges of a node's control values.
n_setn :: Int -> [(String,[Float])] -> Message
n_setn = G.n_setn

-- | Create a new synth.
s_new :: String -> Int -> AddAction -> Int -> [(String,Float)] -> Message
s_new = G.s_new

-- | Fill ranges of sample values.
b_fill :: Int -> [(Int,Int,Float)] -> Message
b_fill = G.b_fill

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: Int -> [B_Gen] -> [Float] -> Message
b_gen_sine1 = G.b_gen_sine1

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: Int -> [B_Gen] -> [(Float,Float)] -> Message
b_gen_sine2 = G.b_gen_sine2

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: Int -> [B_Gen] -> [(Float,Float,Float)] -> Message
b_gen_sine3 = G.b_gen_sine3

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: Int -> [B_Gen] -> [Float] -> Message
b_gen_cheby = G.b_gen_cheby

-- | Set sample values.
b_set :: Int -> [(Int,Float)] -> Message
b_set = G.b_set

-- | Set ranges of sample values.
b_setn :: Int -> [(Int,[Float])] -> Message
b_setn = G.b_setn

-- |  Fill ranges of bus values.
c_fill :: [(Int,Int,Float)] -> Message
c_fill = G.c_fill

-- | Set bus values.
c_set :: [(Int,Float)] -> Message
c_set = G.c_set

-- | Set ranges of bus values.
c_setn :: [(Int,[Float])] -> Message
c_setn = G.c_setn

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: Int -> Int -> [Float] -> Message
b_alloc_setn1 = G.b_alloc_setn1

-- | Set single sample value.
b_set1 :: Int -> Int -> Float -> Message
b_set1 = G.b_set1

-- | Set a range of sample values.
b_setn1 :: Int -> Int -> [Float] -> Message
b_setn1 = G.b_setn1

-- | Set single bus values.
c_set1 :: Int -> Float -> Message
c_set1 = G.c_set1

-- | Set a single node control value.
n_set1 :: Int -> String -> Float -> Message
n_set1 = G.n_set1
