module Memory where

import Address
import ExceptionTable

data Memory = InstallMemory {
    exceptionTable  :: ExceptionTable
    -- TODO: add the other memory zone
}
