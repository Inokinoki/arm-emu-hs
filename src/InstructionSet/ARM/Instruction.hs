module InstructionSet.ARM.Instruction where

import Data.Bits
import Numeric
import Data.Word

import Instruction

import InstructionSet.Instruction

-- Debug purpose, wait to be replaced
-- type Instruction = [Char]

-- ARM ISA format:
--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Data Processing Immediate shift:|    cond[1]    | 0 | 0 | 0 |     opcode    | S |       Rn      |       Rd      |    shift amount   | shift | 0 |       Rm      |
--  Data Processing register shift: |    cond[1]    | 0 | 0 | 0 |     opcode    | S |       Rn      |       Rd      |       Rs      | 0 | shift | 1 |       Rm      |
--  Data Processing Immediat:       |    cond[1]    | 0 | 0 | 1 |     opcode    | S |       Rn      |       Rd      |     Rotate    |           Immediate           |
decodeDataProcessing :: Word32 -> Maybe Instruction
decodeDataProcessing x
    | x .&. 0x0E000000 == 0x00000000 = Nothing
    | x .&. 0x0E000000 == 0x02000000 = Nothing
    | otherwise = error $ "Undefined ARM Data Processing instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Load/Store Immediate Offset:    |    cond[1]    | 0 | 1 | 0 | P | U | B | W | L |       Rn      |       Rd      |                  Immediate                    |
--  Load/Store Register Offset:     |    cond[1]    | 0 | 1 | 1 |     opcode    | S |       Rn      |       Rd      |    shift amount   | shift | 0 |       Rm      |
decodeLoadStoreWithOffset :: Word32 -> Maybe Instruction
decodeLoadStoreWithOffset x
    | x .&. 0x0E000000 == 0x04000000 = Nothing
    | x .&. 0x0E000000 == 0x06000000 = Nothing
    | otherwise = error $ "Undefined ARM Load/Store instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Load/Store Multiple:            |    cond[1]    | 1 | 0 | 0 | P | U | B | W | L |       Rn      |                      Register List                            |
decodeLoadStoreMultiple :: Word32 -> Maybe Instruction
decodeLoadStoreMultiple x
    | x .&. 0x0E000000 == 0x08000000 = Nothing
    | otherwise = error $ "Undefined ARM Load/Store Multiple instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Load/Store half word offset:    |    cond[1]    | 0 | 0 | 0 | P | U | B | W | L |       Rn      |       Rd      | SBZ/HiOffset  | 1   0   1   1 |  Rm/LoOffset  |
--  Load/Store two words offset:    |    cond[1]    | 0 | 0 | 0 | P | U | B | W | 0 |       Rn      |       Rd      | SBZ/HiOffset  | 1   1 | S | 1 |  Rm/LoOffset  |
--  Load/Store signed/byte offset:  |    cond[1]    | 0 | 0 | 0 | P | U | B | W | 1 |       Rn      |       Rd      | SBZ/HiOffset  | 1   1 | H | 1 |  Rm/LoOffset  |
decodeLoadStoreMisc :: Word32 -> Maybe Instruction
decodeLoadStoreMisc x
    | x .&. 0x0E0000F0 == 0x000000B0  = Nothing
    | x .&. 0x0E1000D0 == 0x000000D0  = Nothing
    | x .&. 0x0E1000D0 == 0x001000D0  = Nothing
    | otherwise = error $ "Undefined ARM Load/Store Misc instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Branch and branch with link:    |    cond[1]    | 1 | 0 | 1 | L |                                        24-bit Offset                                          |
--  Branch with link and to Thumb:  | 1   1   1   1 | 1 | 0 | 1 | H |                                        24-bit Offset                                          |
decodeBranch :: Word32 -> Maybe Instruction
decodeBranch x
    | x .&. 0x0E0000D0 == 0x0A000000  = Nothing
    | x .&. 0xFE0000D0 == 0xFA000000  = Nothing
    | otherwise = error $ "Undefined ARM Branch instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Branch and to Thumb:            |    cond[1]    | 0   0   0   1   0 | 0   1 | 0 |      SBO      |      SBO      |      SBO      | 0   0   0   1 |       Rm      |
decodeBranchExchange :: Word32 -> Maybe Instruction
decodeBranchExchange x
    | x .&. 0x0FF000F0 == 0x01200010  = Nothing
    | otherwise = error $ "Undefined ARM Branch Exchange instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Multiply (accumulate):          |    cond[1]    | 0   0   0   0 | 0   0 | A | S |       Rd      |       Rn      |       Rs      | 1   0   0   1 |       Rm      |
--  Multiply (accumulate) long:     |    cond[1]    | 0   0   0   0 | 1 | U | A | S |       Rd      |       Rn      |       Rs      | 1   0   0   1 |       Rm      |
decodeMultiplyAccumulate :: Word32 -> Maybe Instruction
decodeMultiplyAccumulate x
    | x .&. 0x0FC000F0 == 0x00000090  = Nothing
    | x .&. 0x0F8000F0 == 0x00800090  = Nothing
    | otherwise = error $ "Undefined ARM Multiply Accumulate instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Move Immediate to status reg:   |    cond[1]    | 0   0   1 | 1   0 | R | 1   0 |      Mask     |      SBO      |     rotate    |           Immediate           |
decodeMSRImmediate :: Word32 -> Maybe Instruction
decodeMSRImmediate x
    | x .&. 0x0FB00000 == 0x03200000  = Nothing
    | otherwise = error $ "Undefined ARM MSR immediate instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Move reg to status reg:         |    cond[1]    | 0   0   0   1   0 | R | 1 | 0 |      Mask     |      SBO      |      SBZ      | 0   0   0   0 |       Rm      |
decodeMSRRegister :: Word32 -> Maybe Instruction
decodeMSRRegister x
    | x .&. 0x0FB000F0 == 0x01200000  = Nothing
    | otherwise = error $ "Undefined ARM MSR reg instruction: " ++ showHex x ""

--                          Format  |31 |30 |29 |28 |27 |26 |25 |24 |23 |22 |21 |20 |19 |18 |17 |16 |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Move status reg to reg:         |    cond[1]    | 0   0   0   1   0 | R | 0 | 0 |      SBO      |       Rd      |      SBZ      | 0   0   0   0 |      SBZ      |
decodeMRS :: Word32 -> Maybe Instruction
decodeMRS x
    | x .&. 0x0FB000F0 == 0x01000000  = Nothing
    | otherwise = error $ "Undefined ARM MRS instruction: " ++ showHex x ""
