module ExceptionTable where

import Address

-- Exception table, should be loaded into 0x00000000
data ExceptionTable = ExceptionTable {
    reset_Hanlder                   :: Address,
    undefinedInstruction_Hanlder    :: Address,
    softwareInterrupt_Hanlder       :: Address,
    prefetchAbort_Hanlder           :: Address,
    dataAbort_Hanlder               :: Address,
    irq_Hanlder                     :: Address,
    fiq_Hanlder                     :: Address
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
