module Memory where

import Address
import Data.Word
import ExceptionTable

import qualified Data.Map.Strict as Map

data Memory = InstallMemory {
    exceptionTable  :: ExceptionVector,
    ram             :: (Map.Map Address Word32)
    -- TODO: add the other memory zone
}

-- TODO: Divide more memory areas
-- instance MemoryRegion Memory where
--     read8 a m = case (a `shiftR` 0x18) of
--             0x00 -> read8 a (exceptionTable m)
--             _    -> read8 a (ram m)
--     write8 a b m = case (a `shiftR` 0x18) of
--             0x00 -> m { exceptionTable = write8 a b (exceptionTable m) }
--             _    -> m { ram = write8 a b (ram m) }
