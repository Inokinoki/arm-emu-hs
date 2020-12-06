module Instruction where

import Data.Word
import Data.Bits


-- TODO: replace with the implementations in Instruction Set
type ThumbInstruction   =   Word16
type ARMInstruction     =   Word32

data Instruction = ThumbInstruction | ARMInstruction

-- decode ARM
decodeARM :: Word32 -> Maybe Instruction
decodeARM x = error $ "Unimplemented decodeARM"

-- decode Thumb
decodeThumb :: Word16 -> Maybe Instruction
decodeThumb x = error $ "Unimplemented decodeTHUMB"

-- declare a typeclass Decode with a decodeWord method
class Decode a where
    decodeWord :: a -> Maybe Instruction

-- implement decode method for Word32 as decodeWord ARM
instance Decode Word32 where
    decodeWord x = decodeARM x

-- implement decode method for Word32 as decodeWord THUMB
instance Decode Word16 where
    decodeWord x = decodeThumb x
