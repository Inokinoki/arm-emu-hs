module Instruction where

import Data.Word
import Data.Bits


-- TODO: replace with the implementations in Instruction Set
type ThumbInstruction   =   Word16
type ARMInstruction     =   Word32

data Instruction = ThumbInstruction | ARMInstruction
