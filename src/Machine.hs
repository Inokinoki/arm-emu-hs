module Machine where

import Core
import Memory

data Machine = MakeMachine {
    cpu         :: Core,
    memory      :: Memory
}
