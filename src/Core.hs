module Core where

import Data.Word
import Data.Bits

type Register = Word32

data Core = Core {
  r0        :: Register,
  r1        :: Register,
  r2        :: Register,
  r3        :: Register,
  r4        :: Register,
  r5        :: Register,
  r6        :: Register,
  r7        :: Register,
  r8        :: Register,
  r9        :: Register,
  r10       :: Register,
  r11       :: Register,
  r12       :: Register,
  r13       :: Register,
  r14       :: Register,
  r15       :: Register,
  cpsr      :: Register
} deriving (Eq)
