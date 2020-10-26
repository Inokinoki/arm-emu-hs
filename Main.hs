module Main where

import Core
import Memory
import Machine
import ExceptionTable

main :: IO ()
main = do
    -- Create a machine
    let m = MakeMachine {
        cpu = InstallCore {
            r0  = 0, r1  = 0, r2  = 0, r3  = 0,
            r4  = 0, r5  = 0, r6  = 0, r7  = 0,
            r8  = 0, r9  = 0, r10 = 0, r11 = 0,
            r12 = 0, r13 = 0, r14 = 0, r15 = 0,
            cpsr = 0
        },
        memory = InstallMemory {
            exceptionTable = ExceptionTable {
                reset_Hanlder                   = 0,
                undefinedInstruction_Hanlder    = 0,
                softwareInterrupt_Hanlder       = 0,
                prefetchAbort_Hanlder           = 0,
                dataAbort_Hanlder               = 0,
                irq_Hanlder                     = 0,
                fiq_Hanlder                     = 0
            }
        }
    }
    -- A cycle
    let i = fetch m
    let op = decode m i
    let m2 = execute m op
    putStrLn "Hello, ARM Core!"
