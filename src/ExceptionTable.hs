module ExceptionTable where

import Address

-- Exception Vector, should be loaded into 0x00000000
data ExceptionVector = ExceptionVector {
    reset_handler                   :: Address,
    undefinedInstruction_handler    :: Address,
    softwareInterrupt_handler       :: Address,
    prefetchAbort_handler           :: Address,
    dataAbort_handler               :: Address,
    irq_handler                     :: Address,
    fiq_handler                     :: Address
} deriving (Eq, Show)

-- Exception type
data Exception = Reset_Exception
                | UndefinedInstruction_Exception
                | SoftwareInterrupt_Exception
                | PrefetchAbort_Exception
                | DataAbort_Exception
                | IRQ_Exception
                | FIQ_Exception
  deriving (Show, Eq)
