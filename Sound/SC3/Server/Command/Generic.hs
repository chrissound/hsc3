-- | Generic constructors for the command set implemented by the SuperCollider synthesis server.
module Sound.SC3.Server.Command.Generic where

import           Data.List
import           Data.Maybe

import           Sound.OSC.Core

import qualified Sound.SC3.Common.Prelude      as P
import qualified Sound.SC3.Server.Command.Enum as C
import qualified Sound.SC3.Server.Enum         as E
import qualified Sound.SC3.Server.Graphdef     as G
import qualified Sound.SC3.Server.Synthdef     as S

-- * Buffer commands (b_)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: Integral i
           => i -- ^ buffer number
           -> i -- ^ number of frames
           -> i -- ^ number of channels
           -> Message
b_alloc nid frames channels =
  message "/b_alloc" [ int32 nid
                     , int32 frames
                     , int32 channels
                     ]

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: Integral i
            => i      -- ^ buffer number
            -> String -- ^ path name of a sound file
            -> i      -- ^ starting frame in file
            -> i      -- ^ number of frames to read
            -> Message
b_allocRead nid path start frames =
  message "/b_allocRead" [ int32 nid
                         , string path
                         , int32 start
                         , int32 frames
                         ]

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: Integral i
                      => i      -- ^ buffer number
                      -> String -- ^ path name of a sound file
                      -> i      -- ^ starting frame in file
                      -> i      -- ^ number of frames to read
                      -> [i]    -- ^ source file channel indexes
                      -> Message
b_allocReadChannel nid path start frames channels =
  message "/b_allocReadChannel" $ [ int32 nid
                                  , string path
                                  , int32 start
                                  , int32 frames
                                  ] ++ map int32 channels

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Integral i
           => i -- ^ buffer number
           -> Message
b_close nid = message "/b_close" [int32 nid]

-- | Fill ranges of sample values.
b_fill :: (Integral i, Real n)
          => i           -- ^ buffer number
          -> [(i, i, n)] -- ^ sample starting index, number of samples to fill, value
          -> Message
b_fill nid xs = message "/b_fill" (int32 nid : P.mk_triples int32 int32 float xs)

-- | Free buffer data. (Asynchronous)
b_free :: Integral i
       => i -- ^ buffer number
       -> Message
b_free nid = message "/b_free" [int32 nid]

-- | Call a command to fill a buffer.  (Asynchronous)
b_gen :: Integral i
      => i       -- ^ buffer number
      -> String  -- ^ command name
      -> [Datum] -- ^ command arguments
      -> Message
b_gen nid name xs = message "/b_gen" (int32 nid : string name : xs)

-- | Call @sine1@ 'b_gen' command.
b_gen_sine1 :: (Integral i, Real n)
            => i         -- ^ buffer number
            -> [E.B_Gen]
            -> [n]
            -> Message
b_gen_sine1 nid flag n =
  b_gen nid "sine1"
  $ int32 (E.b_gen_flag flag) : map float n

-- | Call @sine2@ 'b_gen' command.
b_gen_sine2 :: (Integral i, Real n)
            => i         -- ^ buffer number
            -> [E.B_Gen]
            -> [(n, n)]
            -> Message
b_gen_sine2 nid flag n =
  b_gen nid "sine2"
  $ int32 (E.b_gen_flag flag) : P.mk_duples float float n

-- | Call @sine3@ 'b_gen' command.
b_gen_sine3 :: (Integral i, Real n)
            => i           -- ^ buffer number
            -> [E.B_Gen]
            -> [(n, n, n)]
            -> Message
b_gen_sine3 nid flag n =
  b_gen nid "sine3"
  $ int32 (E.b_gen_flag flag) : P.mk_triples float float float n

-- | Call @cheby@ 'b_gen' command.
b_gen_cheby :: (Integral i, Real n)
            => i         -- ^ buffer number
            -> [E.B_Gen]
            -> [n]
            -> Message
b_gen_cheby nid flag n =
  b_gen nid "cheby"
  $ int32 (E.b_gen_flag flag) : map float n

-- | Call @copy@ 'b_gen' command.
b_gen_copy :: Integral i
           => i
           -> i
           -> i
           -> i
           -> Maybe i
           -> Message
b_gen_copy dst_b dst_ix src_b src_ix nf =
  b_gen dst_b "copy"
  $ map int32 [ dst_ix
              , src_b
              , src_ix
              , fromMaybe (-1) nf
              ]

-- | Get sample values.
b_get :: Integral i
      => i   -- ^ buffer number
      -> [i] -- ^ sample indexes
      -> Message
b_get nid xs = message "/b_get" (int32 nid : map int32 xs)

-- | Get ranges of sample values.
b_getn :: Integral i
       => i        -- ^ buffer number
       -> [(i, i)] -- ^ starting sample index, number of sequential samples to get
       -> Message
b_getn nid xs = message "/b_getn" (int32 nid : P.mk_duples int32 int32 xs)

-- | Request \/b_info messages.
b_query :: Integral i
        => [i] -- ^ buffer numbers
        -> Message
b_query = message "/b_query" . map int32

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Integral i
       => i      -- ^ buffer number
       -> String -- ^ path name of a sound file
       -> i      -- ^ starting frame in file
       -> i      -- ^ number of frames to read
       -> i      -- ^ starting frame in buffer
       -> Bool   -- ^ leave file open?
       -> Message
b_read nid path fileStart frames bufStart leaveOpen =
  message "/b_read" [ int32 nid
                    , string path
                    , int32 fileStart
                    , int32 frames
                    , int32 bufStart
                    , int32 (fromEnum leaveOpen)
                    ]

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: Integral i
              => i      -- ^ buffer number
              -> String -- ^ path name of a sound file
              -> i      -- ^ starting frame in file
              -> i      -- ^ number of frames to read
              -> i      -- ^ starting frame in buffer
              -> Bool   -- ^ leave file open?
              -> [i]    -- ^ source file channel indexes
              -> Message
b_readChannel nid path fileStart frames bufStart leaveOpen channels =
  message "/b_readChannel" $ [ int32 nid
                             , string path
                             , int32 fileStart
                             , int32 frames
                             , int32 bufStart
                             , int32 (fromEnum leaveOpen)
                             ] ++ map int32 channels

-- | Set sample values.
b_set :: (Integral i, Real n)
      => i        -- ^ buffer number
      -> [(i, n)] -- ^ sample index, sample value
      -> Message
b_set nid xs = message "/b_set" (int32 nid : P.mk_duples int32 float xs)

-- | Set ranges of sample values.
b_setn :: (Integral i, Real n)
       => i          -- ^ buffer number
       -> [(i, [n])] -- ^ sample starting index, sample values
       -> Message
b_setn nid ranges =
  let
    format (i, xs) = int32 i : int32 (length xs) : map float xs
  in
    message "/b_setn" (int32 nid : concatMap format ranges)

-- | Write sound file data. (Asynchronous)
b_write :: Integral i
        => i                 -- ^ buffer number
        -> String            -- ^ path name of a sound file
        -> E.SoundFileFormat -- ^ header format
        -> E.SampleFormat    -- ^ sample format
        -> i                 -- ^ number of frames to write
        -> i                 -- ^ starting frame in buffer
        -> Bool              -- ^ leave file open?
        -> Message
b_write nid path headerFormat sampleFormat frames start leaveOpen =
  message "/b_write" [ int32 nid
                     , string path
                     , string (E.soundFileFormatString headerFormat)
                     , string (E.sampleFormatString sampleFormat)
                     , int32 frames
                     , int32 start
                     , int32 (fromEnum leaveOpen)
                     ]

-- | Zero sample data. (Asynchronous)
b_zero :: Integral i
       => i -- ^ buffer number
       -> Message
b_zero nid = message "/b_zero" [int32 nid]

-- * Control bus commands (c_)

-- |  Fill ranges of bus values.
c_fill :: (Integral i, Real n)
       => [(i, i, n)] -- ^ starting bus index, number of buses to fill, value
       -> Message
c_fill = message "/c_fill" . P.mk_triples int32 int32 float

-- | Get bus values.
c_get :: Integral i
      => [i] -- ^ bus indexes
      -> Message
c_get = message "/c_get" . map int32

-- | Get ranges of bus values.
c_getn :: Integral i
       => [(i, i)] -- ^ starting bus index, number of sequential buses to get
       -> Message
c_getn = message "/c_getn" . P.mk_duples int32 int32

-- | Set bus values.
c_set :: (Integral i, Real n)
      => [(i, n)] -- ^ bus index, control value
      -> Message
c_set = message "/c_set" . P.mk_duples int32 float

-- | Set ranges of bus values.
c_setn :: (Integral i, Real n)
       => [(i, [n])] -- ^ starting bus index, control values
       -> Message
c_setn ranges =
    let
      format (i, xs) = int32 i : int32 (length xs) : map float xs
    in
      message "/c_setn" (concatMap format ranges)

-- * Instrument definition commands (d_)

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv' :: G.Graphdef -- ^ buffer of data
        -> Message
d_recv' g = message "/d_recv" [Blob (G.encode_graphdef g)]

-- | Install a bytecode instrument definition. (Asynchronous)
d_recv :: S.Synthdef -- ^ buffer of data
       -> Message
d_recv d = message "/d_recv" [Blob (S.synthdefData d)]

-- | Load an instrument definition from a named file. (Asynchronous)
d_load :: String -- ^ pathname of file
       -> Message
d_load path = message "/d_load" [string path]

-- | Load a directory of instrument definitions files. (Asynchronous)
d_loadDir :: String -- ^ pathname of directory
          -> Message
d_loadDir path = message "/d_loadDir" [string path]

-- | Remove definition once all nodes using it have ended.
d_free :: [String] -- ^ SynthDef names
       -> Message
d_free = message "/d_free" . map string

-- * Group node commands (g_)

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: Integral i
           => [i] -- ^ group IDs
           -> Message
g_deepFree = message "/g_deepFree" . map int32

-- | Delete all nodes in a group.
g_freeAll :: Integral i
          => [i] -- ^ group IDs
          -> Message
g_freeAll = message "/g_freeAll" . map int32

-- | Add node to head of group.
g_head :: Integral i
       => [(i, i)] -- ^ group ID, node ID
       -> Message
g_head = message "/g_head" . P.mk_duples int32 int32

-- | Create a new group.
g_new :: Integral i
      => [(i, E.AddAction, i)] -- ^ new group ID, add action, add target ID
      -> Message
g_new = message "/g_new" . P.mk_triples int32 (int32 . fromEnum) int32

-- | Add node to tail of group.
g_tail :: Integral i
       => [(i, i)] -- ^ group ID, node ID
       -> Message
g_tail = message "/g_tail" . P.mk_duples int32 int32

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: Integral i
           => [(i, Bool)] -- ^ group ID, flag
           -> Message
g_dumpTree = message "/g_dumpTree" . P.mk_duples int32 (int32 . fromEnum)

-- | Request a representation of a group's node subtree, optionally including the current control values for synths.
--
-- Replies to the sender with a @/g_queryTree.reply@ message listing all of the nodes contained within the group in the following format:
--
-- > int32 - if synth control values are included 1, else 0
-- > int32 - node ID of the requested group
-- > int32 - number of child nodes contained within the requested group
-- >
-- > For each node in the subtree:
-- > [
-- >   int32 - node ID
-- >   int32 - number of child nodes contained within this node. If -1 this is a synth, if >= 0 it's a group.
-- >
-- >   If this node is a synth:
-- >     symbol - the SynthDef name for this node.
-- >
-- >   If flag (see above) is true:
-- >     int32 - numControls for this synth (M)
-- >     [
-- >       symbol or int: control name or index
-- >       float or symbol: value or control bus mapping symbol (e.g. 'c1')
-- >     ] * M
-- > ] * the number of nodes in the subtree
--
-- N.B. The order of nodes corresponds to their execution order on the server. Thus child nodes (those contained within a group) are listed immediately following their parent.
g_queryTree :: Integral i
            => [(i, Bool)] -- ^ group ID, flag
            -> Message
g_queryTree = message "/g_queryTree" . P.mk_duples int32 (int32 . fromEnum)

-- * Node commands (n_)

-- | Place a node after another.
n_after :: Integral i
        => [(i, i)] -- ^ ID of node to place, ID of node after which the chosen node is placed
        -> Message
n_after = message "/n_after" . P.mk_duples int32 int32

-- | Place a node before another.
n_before :: Integral i
         => [(i, i)] -- ^ ID of node to place, ID of node before which the chosen node is placed
         -> Message
n_before = message "/n_before" . P.mk_duples int32 int32

-- | Fill ranges of a node's control values.
n_fill :: (Integral i, Real f)
       => i                -- ^ node ID
       -> [(String, i, f)] -- ^ control index/name, number of values to fill, value
       -> Message
n_fill nid xs = message "/n_fill" (int32 nid : P.mk_triples string int32 float xs)

-- | Delete nodes.
n_free :: Integral i
       => [i] -- ^ node IDs
       -> Message
n_free = message "/n_free" . map int32

-- | Map a node's controls to read from a bus.
n_map :: Integral i
      => i             -- ^ node ID
      -> [(String, i)] -- ^ control index/name, control bus index
      -> Message
n_map nid xs = message "/n_map" (int32 nid : P.mk_duples string int32 xs)

-- | Map a node's controls to read from buses.
n_mapn :: Integral i
       => i                -- ^ node ID
       -> [(String, i, i)] -- ^ control index/name, control bus index, number of controls to map
       -> Message
n_mapn nid xs = message "/n_mapn" (int32 nid : P.mk_triples string int32 int32 xs)

-- | Map a node's controls to read from an audio bus.
n_mapa :: Integral i
       => i             -- ^ node ID
       -> [(String, i)] -- ^ control index/name, control bus index
       -> Message
n_mapa nid xs = message "/n_mapa" (int32 nid : P.mk_duples string int32 xs)

-- | Map a node's controls to read from audio buses.
n_mapan :: Integral i
        => i                -- ^ node ID
        -> [(String, i, i)] -- ^ control index/name, control bus index, number of controls to map
        -> Message
n_mapan nid xs = message "/n_mapan" (int32 nid : P.mk_triples string int32 int32 xs)

-- | Get info about nodes.
n_query :: Integral i
        => [i] -- ^ node IDs
        -> Message
n_query = message "/n_query" . map int32

-- | Turn nodes on or off.
n_run :: Integral i
      => [(i, Bool)] -- ^ node ID, run flag
      -> Message
n_run = message "/n_run" . P.mk_duples int32 (int32 . fromEnum)

-- | Set a node's control values.
n_set :: (Integral i, Real n)
      => i             -- ^ node ID
      -> [(String, n)] -- ^ control index/name, control value
      -> Message
n_set nid xs = message "/n_set" (int32 nid : P.mk_duples string float xs)

-- | Set ranges of a node's control values.
n_setn :: (Integral i, Real n)
       => i               -- ^ node ID
       -> [(String, [n])] -- ^ control index/name, control values
       -> Message
n_setn nid controls =
    let
      format (i, xs) = string i : int32 (length xs) : map float xs
    in
      message "/n_setn" (int32 nid : concatMap format controls)

-- | Trace nodes.
n_trace :: Integral i
        => [i] -- ^ node IDs
        -> Message
n_trace = message "/n_trace" . map int32

-- | Move an ordered sequence of nodes.
n_order :: Integral i
        => E.AddAction -- ^ add action
        -> i           -- ^ add target ID
        -> [i]         -- ^ node IDs
        -> Message
n_order addAction target xs =
  message "/n_order" $ int32 (fromEnum addAction) : int32 target : map int32 xs

-- * Par commands (p_)

-- | Create a new parallel group (supernova specific).
p_new :: Integral i
      => [(i, E.AddAction, i)] -- ^ new group ID, add action, add target ID
      -> Message
p_new = message "/p_new" . P.mk_triples int32 (int32 . fromEnum) int32

-- * Synthesis node commands (s_)

-- | Get control values.
s_get :: Integral i
      => i        -- ^ synth ID
      -> [String] -- ^ control index/name
      -> Message
s_get nid xs = message "/s_get" (int32 nid : map string xs)

-- | Get ranges of control values.
s_getn :: Integral i
       => i             -- ^ synth ID
       -> [(String, i)] -- ^ control index/name, number of sequential controls to get
       -> Message
s_getn nid xs = message "/s_getn" (int32 nid : P.mk_duples string int32 xs)

-- | Create a new synth.
s_new :: (Integral i, Real n)
      => String        -- ^ synth definition name
      -> i             -- ^ synth ID
      -> E.AddAction   -- ^ add action
      -> i             -- ^ add target ID
      -> [(String, n)] -- ^ control index/name, control value
      -> Message
s_new name synthId addAction targetId controls =
  message "/s_new"
  $ string name
  : int32 synthId
  : int32 (fromEnum addAction)
  : int32 targetId
  : P.mk_duples string float controls

-- | Auto-reassign synth's ID to a reserved value.
s_noid :: Integral i
       => [i] -- ^ synth IDs
       -> Message
s_noid = message "/s_noid" . map int32

-- * UGen commands (u_)

-- | Send a command to a unit generator.
u_cmd :: Integral i
      => i       -- ^ node ID
      -> i       -- ^ unit generator index
      -> String  -- ^ command name
      -> [Datum] -- ^ arguments
      -> Message
u_cmd nid ugenId name args =
  message "/u_cmd" $ [ int32 nid
                     , int32 ugenId
                     , string name
                     ] ++ args

-- * Server operation commands

-- | Send a plugin command.
cmd :: String  -- ^ command name
    -> [Datum] -- ^ arguments
    -> Message
cmd name xs = message "/cmd" (string name : xs)

-- | Remove all bundles from the scheduling queue.
clearSched :: Message
clearSched = message "/clearSched" []

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: E.PrintLevel -- ^ code
        -> Message
dumpOSC c = message "/dumpOSC" [int32 (fromEnum c)]

-- | Set error posting scope and mode.
errorMode :: E.ErrorScope -- ^ scope
          -> E.ErrorMode  -- ^ mode
          -> Message
errorMode scope mode =
    let
      errMode = case scope of
                  E.Globally -> fromEnum mode
                  E.Locally  -> -1 - fromEnum mode
    in
      message "/error" [int32 errMode]

-- | Select reception of notification messages. (Asynchronous)
notify :: Bool    -- ^ receive notifications?
       -> Message
notify c = message "/notify" [int32 (fromEnum c)]

-- | End real time mode, close file (un-implemented).
nrt_end :: Message
nrt_end = message "/nrt_end" []

-- | Stop synthesis server.
quit :: Message
quit = message "/quit" []

-- | Request \/status.reply message.
status :: Message
status = message "/status" []

-- | Request \/synced message when all current asynchronous commands complete.
sync :: Integral i
     => i -- ^ a unique number identifying this command
     -> Message
sync sid = message "/sync" [int32 sid]

-- * Modify existing message to include completion message

-- | Add a completion message (or bundle, the name is misleading) to
-- an existing asynchronous command.
--
-- >  let
-- >    msg    = n_set1 0 "0" 0
-- >    encMsg = encodeMessage m
-- >  in
-- >    withCM (b_close 0) msg == Message "/b_close" [Int 0, Blob encMsg]
withCM :: OSC o
       => Message
       -> o -- ^ completion message
       -> Message
withCM (Message command params) completionMsg =
    if command `elem` C.async_cmds
    then Message command (params ++ [Blob (encodeOSC completionMsg)])
    else error ("withCM: not async: " ++ command)

-- * Variants to simplify common cases

-- | Pre-allocate for b_setn1, values preceding offset are zeroed.
b_alloc_setn1 :: (Integral i, Real n)
              => i   -- ^ node ID
              -> i   -- ^ offset
              -> [n] -- ^ samples
              -> Message
b_alloc_setn1 nid offset samples =
    let
      frames        = offset + genericLength samples
      offsetSamples = genericReplicate offset 0 ++ samples
    in
      withCM (b_alloc nid frames 1) (b_setn1 nid 0 offsetSamples)

-- | Get ranges of sample values.
b_getn1 :: Integral i
        => i      -- ^ node ID
        -> (i, i) -- ^ starting sample index, number of sequential samples to get
        -> Message
b_getn1 nid = b_getn nid . return

-- | Variant on 'b_query'.
b_query1 :: Integral i
         => i -- ^ node ID
         -> Message
b_query1 = b_query . return

-- | Set single sample value.
b_set1 :: (Integral i, Real n)
       => i -- ^ buffer number
       -> i -- ^ sample index
       -> n -- ^ sample value
       -> Message
b_set1 nid i x = b_set nid [(i, x)]

-- | Set a range of sample values.
b_setn1 :: (Integral i, Real n)
        => i   -- ^ buffer number
        -> i   -- ^ sample starting index
        -> [n] -- ^ sample values
        -> Message
b_setn1 nid i xs = b_setn nid [(i, xs)]

-- | Segmented variant of 'b_setn1'.
b_setn1_segmented :: (Integral i, Real n)
                  => i         -- ^ segment size
                  -> i         -- ^ buffer ID
                  -> i         -- ^ sample starting index
                  -> [n]       -- ^ sample values
                  -> [Message]
b_setn1_segmented segment bid start samples =
  if genericLength samples < segment
  then
    [b_setn1 bid start samples]
  else
    b_setn1 bid start (genericTake segment samples)
    : b_setn1_segmented segment bid (start + segment) (genericDrop segment samples)

-- | Get ranges of sample values.
c_getn1 :: Integral i
        => (i, i) -- ^ starting bus index, number of sequential buses to get
        -> Message
c_getn1 = c_getn . return

-- | Set single bus values.
c_set1 :: (Integral i, Real n)
       => i -- ^ bus index
       -> n -- ^ control value
       -> Message
c_set1 i x = c_set [(i, x)]

-- | Set single range of bus values.
c_setn1 :: (Integral i, Real n)
        => (i, [n]) -- ^ bus index, control values
        -> Message
c_setn1 = c_setn . return

-- | Turn a single node on or off.
n_run1 :: Integral i
       => i       -- ^ node ID
       -> Bool    -- ^ run flag
       -> Message
n_run1 nid flag = n_run [(nid, flag)]

-- | Set a single node control value.
n_set1 :: (Integral i, Real n)
       => i      -- ^ node ID
       -> String -- ^ control index/name
       -> n      -- ^ control value
       -> Message
n_set1 nid i x = n_set nid [(i, x)]

-- | @s_new@ with no parameters.
s_new0 :: Integral i
       => String      -- ^ synth definition name
       -> i           -- ^ synth ID
       -> E.AddAction -- ^ add action
       -> i           -- ^ add target ID
       -> Message
s_new0 name synthId addAction targetId =
  s_new name synthId addAction targetId ([] :: [(String, Double)])

-- * Buffer segmentation and indices

-- | Segment a request for /m/ places into sets of at most /n/.
--
-- > b_segment 1024 2056 == [8,1024,1024]
-- > b_segment 1 5 == replicate 5 1
b_segment :: Integral i
          => i
          -> i
          -> [i]
b_segment n m =
    let
      (q, r) = m `quotRem` n
      s      = genericReplicate q n
    in
      if r == 0
      then s
      else r : s

-- | Variant of 'b_segment' that takes a starting index and returns
-- /(index,size)/ duples.
--
-- > b_indices 1 5 0 == zip [0..4] (replicate 5 1)
-- > b_indices 1024 2056 16 == [(16,8),(24,1024),(1048,1024)]
b_indices :: Integral i
          => i
          -> i
          -> i
          -> [(i, i)]
b_indices n m k =
    let
      s = b_segment n m
      i = 0 : P.dx_d s
    in
      zip (map (+ k) i) s

-- * UGen commands.

-- | Generate accumulation buffer given time-domain IR buffer and FFT size.
pc_preparePartConv :: Integral i
                   => i
                   -> i
                   -> i
                   -> Message
pc_preparePartConv b irb fft_size =
    b_gen b "PreparePartConv" (map int32 [irb, fft_size])

-- * Unpack

-- | Result is null for non-conforming data, or has five or sevel elements.
unpack_n_info_datum_plain :: Num i
                          => [Datum]
                          -> [i]
unpack_n_info_datum_plain m =
    let
      to_i = fromIntegral
    in
      case m of
        [Int32 i1, Int32 i2, Int32 i3, Int32 i4, Int32 i5] ->
          [to_i i1, to_i i2, to_i i3, to_i i4, to_i i5]
        [Int32 i1, Int32 i2, Int32 i3, Int32 i4, Int32 i5, Int32 i6, Int32 i7] ->
          [to_i i1, to_i i2, to_i i3, to_i i4, to_i i5, to_i i6, to_i i7]
        _ ->
          []

unpack_n_info_plain :: Num i
                    => Message
                    -> [i]
unpack_n_info_plain m =
  case m of
    Message "/n_info" dat -> unpack_n_info_datum_plain dat
    _                     -> []

-- | Unpack @n_info@ message.
unpack_n_info :: Num i
              => Message
              -> Maybe (i, i, i, i, i, Maybe (i, i))
unpack_n_info m =
  case unpack_n_info_plain m of
    [i1,i2,i3,i4,i5]       -> Just (i1,i2,i3,i4,i5,Nothing)
    [i1,i2,i3,i4,i5,i6,i7] -> Just (i1,i2,i3,i4,i5,Just (i6,i7))
    _                      -> Nothing

unpack_n_info_err :: Num i
                  => Message
                  -> (i, i, i, i, i, Maybe (i, i))
unpack_n_info_err = fromMaybe (error "unpack_n_info") . unpack_n_info

-- | Unpack the '/tr' messages sent by 'sendTrig'.
unpack_tr :: (Num i, Fractional f)
          => Message
          -> Maybe (i, i, f)
unpack_tr m =
    let
      to_i = fromIntegral
      to_f = realToFrac
    in
      case m of
         Message "/tr" [Int32 p, Int32 q, Float r] -> Just (to_i p, to_i q, to_f r)
         _ -> Nothing

unpack_tr_err :: (Num i,Fractional f) => Message -> (i,i,f)
unpack_tr_err = fromMaybe (error "unpack_tr") . unpack_tr

unpack_b_setn :: (Num i,Fractional f) => Message -> Maybe (i,i,i,[f])
unpack_b_setn m =
    let to_i = fromIntegral
        to_f d = case d of
                    Float n -> realToFrac n
                    _       -> error "unpack_b_setn: non-float data"
    in case m of
         Message "/b_setn" (Int32 p:Int32 q:Int32 r:z) ->
           Just (to_i p,to_i q,to_i r,map to_f z)
         _ ->
           Nothing

unpack_b_setn_err :: (Num i, Fractional f)
                  => Message
                  -> (i, i, i, [f])
unpack_b_setn_err = fromMaybe (error "unpack_b_setn") . unpack_b_setn

-- | Unpack @b_info@ message, fields are (id,frames,channels,sample-rate).
unpack_b_info :: (Num i, Fractional f)
              => Message
              -> Maybe (i, i, i, f)
unpack_b_info m =
    let
      to_i = fromIntegral
      to_f = realToFrac
    in
      case m of
        Message "/b_info" [Int32 p, Int32 q, Int32 r, Float s] ->
          Just (to_i p, to_i q, to_i r, to_f s)
        _ -> Nothing

-- | Variant generating 'error'.
unpack_b_info_err :: (Num i,Fractional f) => Message -> (i, i, i, f)
unpack_b_info_err = fromMaybe (error "unpack_b_info") . unpack_b_info

-- Local Variables:
-- truncate-lines:t
-- End:
