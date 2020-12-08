module ExceptionTable where

import Address

-- Exception table, should be loaded into 0x00000000
data ExceptionTable = ExceptionTable {
    reset_Handler                   :: Address,
    undefinedInstruction_Handler    :: Address,
    softwareInterrupt_Handler       :: Address,
    prefetchAbort_Handler           :: Address,
    dataAbort_Handler               :: Address,
    irq_Handler                     :: Address,
    fiq_Handler                     :: Address
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
