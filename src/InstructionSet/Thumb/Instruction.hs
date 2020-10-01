module InstructionSet.Thumb.Instruction where

-- Thumb IS format:
--                              Format  |15 |14 |13 |12 |11 |10 | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
--  Move shifted register       01      | 0 | 0 | 0 |  Op   |      Offset5      |     Rs    |    Rd     |
--  Add and subtract            02      | 0 | 0 | 0 | 1 | 1 | 1 |Op |Rn/Offset3 |     Rs    |    Rd     |

--  Move, compare, add, and     03      | 0 | 0 | 1 |  Op   |    Rd     |            Offset8            |
--      subtract immediate


--  ALU operation               04      | 0 | 1 | 0 | 0 | 0 | 0 |       Op      |     Rs    |    Rd     |
--  High register operations    05      | 0 | 1 | 0 | 0 | 0 | 1 |   Op  |H1 |H2 |   Rs/Hs   |    RdHd   |
--      and branch exchange 
--  PC-relative load            06      | 0 | 1 | 0 | 0 | 1 |    Rd     |              Word8            |
--  Load and store(L&S) with    07      | 0 | 1 | 0 | 1 | L | B | 0 |     Ro    |    Rd     |     Rb    |
--      relative offset
--  L&S sign-extended byte      08      | 0 | 1 | 0 | 1 | H | S | 1 |     Ro    |    Rd     |     Rb    |
--      and halfword


--  L&S with immediate offset   09      | 0 | 1 | 1 | B | L |      Offset5      |     Rb    |    Rd     |


--  L&S halfword                10      | 1 | 0 | 0 | 0 | L |      Offset5      |     Rb    |    Rd     |
--  SP-relative L&S             11      | 1 | 0 | 0 | 1 | L |    Rd     |              Word8            |


--  Load address                12      | 1 | 0 | 1 | 0 |SP |    Rd     |              Word8            |
--  Add offset to SP            13      | 1 | 0 | 1 | 1 | 0 | 0 | 0 | 0 | S |           Sword7          |
--  Push and pop registers      14      | 1 | 0 | 1 | 1 | L | 1 | 0 | R |               Rlist           |


--  Multiple L&S                15      | 1 | 1 | 0 | 0 | L |    Rb     |               Rlist           |
--  Conditional branch          16      | 1 | 1 | 0 | 1 |      Cond     |              Softset8         |
--  Software interrupt          17      | 1 | 1 | 0 | 1 | 1 | 1 | 1 | 1 |               Value8          |


--  Unconditional branch        18      | 1 | 1 | 1 | 0 | 0 |                Offset11                   |
--  Long branch with link       19      | 1 | 1 | 1 | 1 | H |                 Offset                    |
