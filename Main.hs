module Main where

import Core
import Memory
import Machine
import ExceptionTable
import Data.Word
import Instruction
import InstructionSet.Decode

import qualified Control.Exception as Exc

-- wrappers for test purpose
word32FromInteger :: Integer -> Word32
word32FromInteger x = fromInteger x

word16FromInteger :: Integer -> Word16
word16FromInteger x = fromInteger x
-- end

main :: IO ()
main = do
    -- Create a machine
    let m = MakeMachine {
        cpu = InstallCore {
            r0  = 0, r1  = 0, r2  = 0, r3  = 0,
            r4  = 0, r5  = 0, r6  = 0, r7  = 0,
            r8  = 0, r9  = 0, r10 = 0, r11 = 0,
            r12 = 0, r13 = 0, r14 = 0, r15 = 0,
            cpsr = 0,
            r13_svc = 0, r14_svc = 0,
            r13_und = 0, r14_und = 0,
            r13_abt = 0, r14_abt = 0,
            r13_irq = 0, r14_irq = 0,
            r8_fiq = 0, r9_fiq = 0, r10_fiq = 0, r11_fiq = 0, r12_fiq = 0, r13_fiq = 0, r14_fiq = 0
        },
        memory = InstallMemory {
            exceptionTable = ExceptionVector {
                reset_handler                   = 0,
                undefinedInstruction_handler    = 0,
                softwareInterrupt_handler       = 0,
                prefetchAbort_handler           = 0,
                dataAbort_handler               = 0,
                irq_handler                     = 0,
                fiq_handler                     = 0
            }
            -- TODO: Add ram field initialization
        }
    }
    -- A cycle
    let i = fetch m
    let op = decode m i
    let m2 = execute m op

    -- test decoding
    let word32 = word32FromInteger 114514
    let decoded32 = decodeWord word32
    let word16 = word16FromInteger 114
    let decoded16 = decodeWord word16

    putStrLn "Hello, ARM Core!"
