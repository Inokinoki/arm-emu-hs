module Main where

import Core
import Memory
import Machine

main :: IO ()
main = do
    let m = MakeMachine
    -- let core = Core {
    --     r0  = 0, r1  = 0, r2  = 0, r3  = 0,
    --     r4  = 0, r5  = 0, r6  = 0, r7  = 0,
    --     r8  = 0, r9  = 0, r10 = 0, r11 = 0,
    --     r12 = 0, r13 = 0, r14 = 0, r15 = 0,
    --     cpsr = 0
    -- }
    putStrLn "Hello, ARM Core!"
