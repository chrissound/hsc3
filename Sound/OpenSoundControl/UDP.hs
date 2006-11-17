module Sound.OpenSoundControl.UDP (UDP, Port, udp, send, recv, wait, close, withUDP) where

import Sound.OpenSoundControl.U8v (u8v_str, str_u8v)
import Sound.OpenSoundControl.OSC (OSC(..), encode, decode)

import Control.Exception(bracket)
import qualified Network.Socket as N

type UDP = N.Socket
type Port = N.PortNumber

-- | Make a UDP connection.
udp :: String -> Port -> IO UDP
udp host port = do fd <- N.socket N.AF_INET N.Datagram 0
                   a  <- N.inet_addr host
                   N.connect fd (N.SockAddrInet port a)
                   -- N.setSocketOption fd N.RecvTimeOut 1000
                   return fd

-- | Encode and send an OSC packet over a UDP connection. 
send :: UDP -> OSC -> IO Int
send fd o = N.send fd (u8v_str (encode o))

-- | Receive and decode an OSC packet over a UDP connection. 
recv :: UDP -> IO OSC
recv fd = do b <- N.recv fd 8192
             return (decode (str_u8v b))

-- | Does the OSC message have the specified address.
hasAddress :: OSC -> String -> Bool
hasAddress (Message s _) addr = s == addr
hasAddress (Bundle _ _)  _    = False

-- | Wait for an OSC message with the specified address, discard intervening messages.
wait :: UDP -> String -> IO OSC
wait fd s = do r <- recv fd
	       if hasAddress r s then return r else wait fd s

-- | Close a UDP connection.
close :: UDP -> IO ()
close = N.sClose

-- | Bracket UDP activity.
withUDP :: IO UDP -> (UDP -> IO a) -> IO a
withUDP u = bracket u close
