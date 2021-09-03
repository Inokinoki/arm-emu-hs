module Memory where

import Address
import ExceptionTable

data Memory = InstallMemory {
    exceptionTable  :: ExceptionVector
    -- TODO: add the other memory zone
}
