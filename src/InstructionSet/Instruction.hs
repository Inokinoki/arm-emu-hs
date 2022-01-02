module InstructionSet.Instruction where

import Data.Word
import Data.Bits

-- TODO: Replace them
type DecodedThumbInstruction    = Word16

-- Can be interpreted as register (4 bits) or register list 
type DecodedARMInstructionRegister = Word16

-- Condition
type DecodedARMInstructionCondition = Word8

-- ARM instructions
data DecodedARMInstructionDataProcessingImmediateShift
    = ARMInstructionDataProcessingImmediateShift DecodedARMInstructionCondition -- Condition
        Word8 Bool DecodedARMInstructionRegister DecodedARMInstructionRegister  -- opcode S Rn, Rd
        Word8 Word8 DecodedARMInstructionRegister                               -- Shift amount, Shift, Rm

type DecodedARMInstruction      = Word32
