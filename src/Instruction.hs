module Instruction where

import Data.Word
import Data.Bits

import InstructionSet.Instruction

-- The implementations are in Instruction Set
data ThumbInstruction = MakeThumbInstruction {
    thumbOrigin :: Word16,
    thumbISA    :: Maybe DecodedThumbInstruction
}

data ARMInstruction = MakeARMInstruction {
    armOrigin   :: Word32,
    armISA      :: Maybe DecodedARMInstruction
}

data Instruction = ThumbInstruction | ARMInstruction
