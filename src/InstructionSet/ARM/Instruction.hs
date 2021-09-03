module InstructionSet.ARM.Instruction where

import Data.Bits
import Numeric
import Data.Word

import Instruction

-- Debug purpose, wait to be replaced
-- type Instruction = [Char]

-- ARM ISA format:
--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Data Processing Immediate shift:|    cond[1]    | 0 | 0 | 0 |     opcode    | S |       Rn      |       Rd      |    shift amount   | shift | 0 |       Rm      |
--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Data Processing register shift: |    cond[1]    | 0 | 0 | 0 |     opcode    | S |       Rn      |       Rd      |       Rs      | 0 | shift | 1 |       Rm      |
--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Data Processing Immediat:       |    cond[1]    | 0 | 0 | 1 |     opcode    | S |       Rn      |       Rd      |     Rotate    |           Immediate           |
decodeDataProcessing :: Word32 -> Maybe Instruction
decodeDataProcessing x
    | x .&. 0x0E000000 == 0x00000000 = Nothing
    | x .&. 0x0E000000 == 0x02000000 = Nothing
    | otherwise = error $ "Undefined ARM Data Processing instruction: " ++ showHex x ""
