module Instruction where

import Data.Word
import Data.Bits


-- TODO: replace with the implementations in Instruction Set
type ThumbInstruction   =   Word16
type ARMInstruction     =   Word32

data Instruction = ThumbInstruction | ARMInstruction

-- decode ARM
-- TODO: reduce cases to match ARM.Instruction
decodeARM :: Word32 -> Maybe Instruction
decodeARM x
    | (x .&. 0x0E000000) == 0x0A000000 = Nothing -- d decodeB
    | (x .&. 0x0FFFFFF0) == 0x012FFF10 = Nothing -- d decodeBX
    | (x .&. 0x0DE00000) == 0x00000000 = Nothing -- d decodeAND
    | (x .&. 0x0DE00000) == 0x00200000 = Nothing -- d decodeEOR
    | (x .&. 0x0DE00000) == 0x00400000 = Nothing -- d decodeSUB
    | (x .&. 0x0DE00000) == 0x00600000 = Nothing -- d decodeRSB
    | (x .&. 0x0DE00000) == 0x00800000 = Nothing -- d decodeADD
    | (x .&. 0x0DE00000) == 0x00A00000 = Nothing -- d decodeADC
    | (x .&. 0x0DE00000) == 0x00C00000 = Nothing -- d decodeSBC
    | (x .&. 0x0DE00000) == 0x00E00000 = Nothing -- d decodeRSC
    | (x .&. 0x0DE0F000) == 0x01000000 = Nothing -- d decodeTST
    | (x .&. 0x0DE0F000) == 0x01200000 = Nothing -- d decodeTEQ
    | (x .&. 0x0DE0F000) == 0x01400000 = Nothing -- d decodeCMP
    | (x .&. 0x0DE0F000) == 0x01600000 = Nothing -- d decodeCMN
    | (x .&. 0x0DE00000) == 0x01800000 = Nothing -- d decodeORR
    | (x .&. 0x0DEF0000) == 0x01A00000 = Nothing -- d decodeMOV
    | (x .&. 0x0DE00000) == 0x01C00000 = Nothing -- d decodeBIC
    | (x .&. 0x0DEF0000) == 0x01E00000 = Nothing -- d decodeMVN
    | (x .&. 0x0FE000F0) == 0x00000090 = Nothing -- d decodeMUL
    | (x .&. 0x0FE000F0) == 0x00200090 = Nothing -- d decodeMLA
    | (x .&. 0x0FE000F0) == 0x00C00090 = Nothing -- d decodeSMULL
    | (x .&. 0x0FE000F0) == 0x00800090 = Nothing -- d decodeUMULL
    | (x .&. 0x0FE000F0) == 0x00E00090 = Nothing -- d decodeSMLAL
    | (x .&. 0x0FE000F0) == 0x00A00090 = Nothing -- d decodeUMLAL
    | (x .&. 0x0FBF0FFF) == 0x010F0000 = Nothing -- d decodeMRS
    | (x .&. 0x0FB0F000) == 0x0320F000 = Nothing -- d decodeMSR_immediate
    | (x .&. 0x0FB0FFF0) == 0x0120F000 = Nothing -- d decodeMSR_register
    | (x .&. 0x0C500000) == 0x04100000 = Nothing -- d decodeLDR
    | (x .&. 0x0C500000) == 0x04500000 = Nothing -- d decodeLDRB
    | (x .&. 0x0D700000) == 0x04700000 = Nothing -- d decodeLDRBT
    | (x .&. 0x0E1000F0) == 0x001000B0 = Nothing -- d decodeLDRH
    | (x .&. 0x0D700000) == 0x04300000 = Nothing -- d decodeLDRT
    | (x .&. 0x0E1000F0) == 0x001000D0 = Nothing -- d decodeLDRSB
    | (x .&. 0x0E1000F0) == 0x001000F0 = Nothing -- d decodeLDRSH
    | (x .&. 0x0C500000) == 0x04000000 = Nothing -- d decodeSTR
    | (x .&. 0x0D700000) == 0x04200000 = Nothing -- d decodeSTRT
    | (x .&. 0x0C500000) == 0x04400000 = Nothing -- d decodeSTRB
    | (x .&. 0x0D700000) == 0x04600000 = Nothing -- d decodeSTRBT
    | (x .&. 0x0E1000F0) == 0x000000B0 = Nothing -- d decodeSTRH
    | (x .&. 0x0E500000) == 0x08100000 = Nothing -- d decodeLDM1
    | (x .&. 0x0E708000) == 0x08500000 = Nothing -- d decodeLDM2
    | (x .&. 0x0E508000) == 0x08508000 = Nothing -- d decodeLDM3
    | (x .&. 0x0E500000) == 0x08000000 = Nothing -- d decodeSTM1
    | (x .&. 0x0E700000) == 0x08400000 = Nothing -- d decodeSTM2
    | otherwise = error $ "Unimplemented decodeARM"

-- decode Thumb
-- TODO: reduce cases to match Thumb.Instruction
decodeThumb :: Word16 -> Maybe Instruction
decodeThumb x
    | x .&. 0xFFC0 == 0x4140 = Nothing -- decodeADC' x
    | x .&. 0xFE00 == 0x1C00 = Nothing -- decodeADD1' x
    | x .&. 0xF800 == 0x3000 = Nothing -- decodeADD2' x
    | x .&. 0xFE00 == 0x1800 = Nothing -- decodeADD3' x
    | x .&. 0xFF00 == 0x4400 = Nothing -- decodeADD4' x
    | x .&. 0xF800 == 0xA000 = Nothing -- decodeADD5' x
    | x .&. 0xF800 == 0xA800 = Nothing -- decodeADD6' x
    | x .&. 0xFF80 == 0xB000 = Nothing -- decodeADD7' x
    | x .&. 0xFFC0 == 0x4000 = Nothing -- decodeAND' x
    | x .&. 0xF800 == 0x1000 = Nothing -- decodeASR1' x
    | x .&. 0xFFC0 == 0x4100 = Nothing -- decodeASR2' x
    | x .&. 0xFF00 == 0xDF00 = Nothing -- decodeSWI' x
    | x .&. 0xF000 == 0xD000 = Nothing -- decodeB1' x
    | x .&. 0xF800 == 0xE000 = Nothing -- decodeB2' x
    | x .&. 0xFFC0 == 0x4380 = Nothing -- decodeBIC' x
    | x .&. 0xF800 == 0xF000 = Nothing -- decodeBLhi' x
    | x .&. 0xF800 == 0xF800 = Nothing -- decodeBLlo' x
    | x .&. 0xFF87 == 0x4700 = Nothing -- decodeBX' x
    | x .&. 0xFFC0 == 0x42C0 = Nothing -- decodeCMN' x
    | x .&. 0xF800 == 0x2800 = Nothing -- decodeCMP1' x
    | x .&. 0xFFC0 == 0x4280 = Nothing -- decodeCMP2' x
    | x .&. 0xFF00 == 0x4500 = Nothing -- decodeCMP3' x
    | x .&. 0xFFC0 == 0x4040 = Nothing -- decodeEOR' x
    | x .&. 0xF800 == 0xC800 = Nothing -- decodeLDMIA' x
    | x .&. 0xF800 == 0x6800 = Nothing -- decodeLDR1' x
    | x .&. 0xFE00 == 0x5800 = Nothing -- decodeLDR2' x
    | x .&. 0xF800 == 0x4800 = Nothing -- decodeLDR3' x
    | x .&. 0xF800 == 0x9800 = Nothing -- decodeLDR4' x
    | x .&. 0xF800 == 0x7800 = Nothing -- decodeLDRB1' x
    | x .&. 0xFE00 == 0x5C00 = Nothing -- decodeLDRB2' x
    | x .&. 0xF800 == 0x8800 = Nothing -- decodeLDRH1' x
    | x .&. 0xFE00 == 0x5A00 = Nothing -- decodeLDRH2' x
    | x .&. 0xFE00 == 0x5600 = Nothing -- decodeLDRSB' x
    | x .&. 0xFE00 == 0x5E00 = Nothing -- decodeLDRSH' x
    | x .&. 0xF800 == 0x0000 = Nothing -- decodeLSL1' x
    | x .&. 0xFFC0 == 0x4080 = Nothing -- decodeLSL2' x
    | x .&. 0xF800 == 0x0800 = Nothing -- decodeLSR1' x
    | x .&. 0xFFC0 == 0x40C0 = Nothing -- decodeLSR2' x
    | x .&. 0xF800 == 0x2000 = Nothing -- decodeMOV1' x
    | x .&. 0xFFC0 == 0x1C00 = Nothing -- decodeMOV2' x
    | x .&. 0xFF00 == 0x4600 = Nothing -- decodeMOV3' x
    | x .&. 0xFFC0 == 0x4340 = Nothing -- decodeMUL' x
    | x .&. 0xFFC0 == 0x43C0 = Nothing -- decodeMVN' x
    | x .&. 0xFFC0 == 0x4240 = Nothing -- decodeNEG' x
    | x .&. 0xFFC0 == 0x4300 = Nothing -- decodeORR' x
    | x .&. 0xFE00 == 0xBC00 = Nothing -- decodePOP' x
    | x .&. 0xFE00 == 0xB400 = Nothing -- decodePUSH' x
    | x .&. 0xFFC0 == 0x41C0 = Nothing -- decodeROR' x
    | x .&. 0xFFC0 == 0x4180 = Nothing -- decodeSBC' x
    | x .&. 0xF800 == 0xC000 = Nothing -- decodeSTMIA' x
    | x .&. 0xF800 == 0x6000 = Nothing -- decodeSTR1' x
    | x .&. 0xFE00 == 0x5000 = Nothing -- decodeSTR2' x
    | x .&. 0xF800 == 0x9000 = Nothing -- decodeSTR3' x
    | x .&. 0xF800 == 0x7000 = Nothing -- decodeSTRB1' x
    | x .&. 0xFE00 == 0x5400 = Nothing -- decodeSTRB2' x
    | x .&. 0xF800 == 0x8000 = Nothing -- decodeSTRH1' x
    | x .&. 0xFE00 == 0x5200 = Nothing -- decodeSTRH2' x
    | x .&. 0xFE00 == 0x1E00 = Nothing -- decodeSUB1' x
    | x .&. 0xF800 == 0x3800 = Nothing -- decodeSUB2' x
    | x .&. 0xFE00 == 0x1A00 = Nothing -- decodeSUB3' x
    | x .&. 0xFF80 == 0xB080 = Nothing -- decodeSUB4' x
    | x .&. 0xFFC0 == 0x4200 = Nothing -- decodeTST' x
    | otherwise = error $ "Unimplemented decodeTHUMB"

-- declare a typeclass Decode with a decodeWord method
class Decode a where
    decodeWord :: a -> Maybe Instruction

-- implement decode method for Word32 as decodeWord ARM
instance Decode Word32 where
    decodeWord x = decodeARM x

-- implement decode method for Word32 as decodeWord THUMB
instance Decode Word16 where
    decodeWord x = decodeThumb x
