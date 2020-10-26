module Machine where

import Core
import Memory
import Instruction

data MachineOperation = CreateMachineOperation Instruction

data Machine = MakeMachine {
    cpu         :: Core,
    memory      :: Memory
}

-- Machine Operations
fetch           :: Machine -> Instruction
fetch m         = ARMInstruction

decode         :: Machine -> Instruction -> MachineOperation
decode m i     = CreateMachineOperation i

execute         :: Machine -> MachineOperation -> Machine
execute m op    = m
