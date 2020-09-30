module Memory where

import Data.Word
import Data.Bits

-- GBA has a ARM7 32bit processor and 32bit address space
type Address = Word32
