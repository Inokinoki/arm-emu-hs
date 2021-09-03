module InstructionSet.Thumb.Instruction where

import Data.Bits
import Numeric
import Data.Word

import Instruction

-- Debug purpose, wait to be replaced
-- type Instruction = [Char]

-- Thumb IS format:
--                              Format  |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Move shifted register       01      | 0 | 0 | 0 |  Op   |      Offset5      |     Rs    |    Rd     |
decodeMoveShiftedRegister :: Word16 -> Maybe Instruction
decodeMoveShiftedRegister x
    | x .&. 0xE000 == 0x0000 = Nothing
    | otherwise = error $ "Undefined THUMB MoveShiftedRegister instruction: " ++ showHex x ""

--  Add and subtract            02      | 0 | 0 | 0 | 1 | 1 | 1 |Op |Rn/Offset3 |     Rs    |    Rd     |
decodeAddSubstract :: Word16 -> Maybe Instruction
decodeAddSubstract x
    | x .&. 0xFC00 == 0x1C00 = Nothing
    | otherwise = error $ "Undefined THUMB AddSubstract instruction: " ++ showHex x ""

--  Move, compare, add, and     03      | 0 | 0 | 1 |  Op   |    Rd     |            Offset8            |
--      subtract immediate
decodeMoveCompareAddSubstractImm :: Word16 -> Maybe Instruction
decodeMoveCompareAddSubstractImm x
    | x .&. 0xE000 == 0x2000 = Nothing
    | otherwise = error $ "Undefined THUMB MoveCompareAddSubstractImm instruction: " ++ showHex x ""


--  ALU operation               04      | 0 | 1 | 0 | 0 | 0 | 0 |       Op      |     Rs    |    Rd     |
decodeALU :: Word16 -> Maybe Instruction
decodeALU x
    | x .&. 0xFC00 == 0x4000 = Nothing
    | otherwise = error $ "Undefined THUMB ALU instruction: " ++ showHex x ""

--  High register operations    05      | 0 | 1 | 0 | 0 | 0 | 1 |   Op  |H1 |H2 |   Rs/Hs   |    RdHd   |
--      and branch exchange 
decodeHighRegisterOperationsAndBranchExchange :: Word16 -> Maybe Instruction
decodeHighRegisterOperationsAndBranchExchange x
    | x .&. 0xFC00 == 0x4400 = Nothing
    | otherwise = error $ "Undefined THUMB HighRegisterOperationsAndBranchExchange instruction: " ++ showHex x ""

--  PC-relative load            06      | 0 | 1 | 0 | 0 | 1 |    Rd     |              Word8            |
decodePCRelativeLoad :: Word16 -> Maybe Instruction
decodePCRelativeLoad x
    | x .&. 0xF800 == 0x4800 = Nothing
    | otherwise = error $ "Undefined THUMB PCRelativeLoad instruction: " ++ showHex x ""

--  Load and store(L&S) with    07      | 0 | 1 | 0 | 1 | L | B | 0 |     Ro    |    Rd     |     Rb    |
--      relative offset
decodeLoadAndStore :: Word16 -> Maybe Instruction
decodeLoadAndStore x
    | x .&. 0xF200 == 0x5000 = Nothing
    | otherwise = error $ "Undefined THUMB LoadAndStore instruction: " ++ showHex x ""

--  L&S sign-extended byte      08      | 0 | 1 | 0 | 1 | H | S | 1 |     Ro    |    Rd     |     Rb    |
--      and halfword
decodeLoadAndStoreSignExtByteAndHalfword :: Word16 -> Maybe Instruction
decodeLoadAndStoreSignExtByteAndHalfword x
    | x .&. 0xF200 == 0x5200 = Nothing
    | otherwise = error $ "Undefined THUMB LoadAndStoreSignExtByteAndHalfword instruction: " ++ showHex x ""


--  L&S with immediate offset   09      | 0 | 1 | 1 | B | L |      Offset5      |     Rb    |    Rd     |
decodeLoadAndStoreWithImmOffset :: Word16 -> Maybe Instruction
decodeLoadAndStoreWithImmOffset x
    | x .&. 0xE000 == 0x6000 = Nothing
    | otherwise = error $ "Undefined THUMB LoadAndStoreWithImmOffset instruction: " ++ showHex x ""


--  L&S halfword                10      | 1 | 0 | 0 | 0 | L |      Offset5      |     Rb    |    Rd     |
decodeLoadAndStoreHalfword :: Word16 -> Maybe Instruction
decodeLoadAndStoreHalfword x
    | x .&. 0xF000 == 0x8000 = Nothing
    | otherwise = error $ "Undefined THUMB LoadAndStoreHalfword instruction: " ++ showHex x ""

--  SP-relative L&S             11      | 1 | 0 | 0 | 1 | L |    Rd     |              Word8            |
decodeSPRelativeLoadAndStore :: Word16 -> Maybe Instruction
decodeSPRelativeLoadAndStore x
    | x .&. 0xF000 == 0x9000 = Nothing
    | otherwise = error $ "Undefined THUMB SPRelativeLoadAndStore instruction: " ++ showHex x ""


--  Load address                12      | 1 | 0 | 1 | 0 |SP |    Rd     |              Word8            |
decodeLoadAddress :: Word16 -> Maybe Instruction
decodeLoadAddress x
    | x .&. 0xF000 == 0xA000 = Nothing
    | otherwise = error $ "Undefined THUMB LoadAddress instruction: " ++ showHex x ""

--  Add offset to SP            13      | 1 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | S |           Sword7          |
decodeOffsetToSP :: Word16 -> Maybe Instruction
decodeOffsetToSP x
    | x .&. 0xFF00 == 0xB000 = Nothing
    | otherwise = error $ "Undefined THUMB OffsetToSP instruction: " ++ showHex x ""

--  Push and pop registers      14      | 1 | 0 | 1 | 1 | L | 1 | 0 | R |               Rlist           |
decodePushAndPopRegisters :: Word16 -> Maybe Instruction
decodePushAndPopRegisters x
    | x .&. 0xF600 == 0xB400 = Nothing
    | otherwise = error $ "Undefined THUMB PushAndPopRegisters instruction: " ++ showHex x ""


--  Multiple L&S                15      | 1 | 1 | 0 | 0 | L |    Rb     |               Rlist           |
decodeMultipleLoadAndStore :: Word16 -> Maybe Instruction
decodeMultipleLoadAndStore x
    | x .&. 0xF000 == 0xC000 = Nothing
    | otherwise = error $ "Undefined THUMB MultipleLoadAndStore instruction: " ++ showHex x ""

--  Conditional branch          16      | 1 | 1 | 0 | 1 |      Cond     |              Softset8         |
decodeConditionalBranch :: Word16 -> Maybe Instruction
decodeConditionalBranch x
    | x .&. 0xF000 == 0xD000 = Nothing
    | otherwise = error $ "Undefined THUMB ConditionalBranch instruction: " ++ showHex x ""

--  Software interrupt          17      | 1 | 1 | 0 | 1 | 1 | 1 | 1 | 1 |               Value8          |
decodeSoftwareIRQ :: Word16 -> Maybe Instruction
decodeSoftwareIRQ x
    | x .&. 0xFF00 == 0xDF00 = Nothing
    | otherwise = error $ "Undefined THUMB SoftwareIRQ instruction: " ++ showHex x ""


--  Unconditional branch        18      | 1 | 1 | 1 | 0 | 0 |                Offset11                   |
decodeUnconditionalBranch :: Word16 -> Maybe Instruction
decodeUnconditionalBranch x
    | x .&. 0xF800 == 0xE000 = Nothing
    | otherwise = error $ "Undefined THUMB UnconditionalBranch instruction: " ++ showHex x ""

--  Long branch with link       19      | 1 | 1 | 1 | 1 | H |                 Offset                    |
decodeLongBranchWithLink :: Word16 -> Maybe Instruction
decodeLongBranchWithLink x
    | x .&. 0xF000 == 0xF000 = Nothing
    | otherwise = error $ "Undefined THUMB LongBranchWithLink instruction: " ++ showHex x ""


-- Register Access:
--  No direct access to CPSR/SPSR (MSR/MRS does not exist)
--  Thumb is flagged by the T bit (bit[5]) in CPSR -> T == 1
