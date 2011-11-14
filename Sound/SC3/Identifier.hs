-- | Typeclass and functions to manage UGen identifiers.
module Sound.SC3.Identifier where

import Data.Char
import qualified Data.Digest.Murmur32 as H

-- | Typeclass to constrain UGen identifiers.
class ID a where
    resolveID :: a -> Int

instance ID Int where
    resolveID = id

instance ID Char where
    resolveID = ord

-- | Hash 'ID' to 'Int'.
idHash :: ID a => a -> Int
idHash = fromIntegral . H.asWord32 . H.hash32 . resolveID

-- | Resolve the ID at 'i' and add the resolved enumeration of 'j'.
editID :: (ID a, Enum b) => a -> b -> Int
editID i j = resolveID i + fromEnum j

-- | Infix alias for editID
(//) :: (ID a, Enum b) => a -> b -> Int
(//) = editID
