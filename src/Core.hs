module Core where

import Data.Word
import Data.Bits

-- Mode:
-- 10000 : User Mode
-- 10001 : FIQ Mode
-- 10010 : IRQ Mode
-- 10011 : Supervisor Mode
-- 10111 : Abort Mode
-- 11011 : Undefined Mode
-- 11111 : System Mode


type Register = Word32

data Core = InstallCore {
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
  cpsr      :: Register,

  r13_svc   :: Register,
  r14_svc   :: Register,

  r13_und   :: Register,
  r14_und   :: Register,

  r13_abt   :: Register,
  r14_abt   :: Register,

  r13_irq   :: Register,
  r14_irq   :: Register,

  r8_fiq    :: Register,
  r9_fiq    :: Register,
  r10_fiq   :: Register,
  r11_fiq   :: Register,
  r12_fiq   :: Register,
  r13_fiq   :: Register,
  r14_fiq   :: Register
} deriving (Eq, Show)
