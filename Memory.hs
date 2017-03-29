module Memory where

import Core
import Base
import Indexed
import Pack
import DFU
import CFU
import DistFunc
import Float

dfuIMemory :: Ptr DIMem -> Maybe Reset
dfuIMemory = encodeDfuI asyncRomFile d512 "dfuIMemory.bin"
dfuDMemory :: Ptr DDMem -> Maybe Float
dfuDMemory = encodeDfuD asyncRomFile d512 "dfuDMemory.bin"
cfuIMemory :: Ptr CIMem -> Maybe Instr
cfuIMemory = encodeCfuI asyncRomFile d512 "cfuIMemory.bin"

fetch :: CoreOut -> CoreIn 
fetch co = CoreIn { nextPack=Nothing
           , dfuInstr=dfuIMemory (dfuIPtr co)
           , dfuData=dfuDMemory (dfuDPtr co)
           , cfuInstr=cfuIMemory (cfuIPtr co)
           }

encodeDfuI :: BitVector 68 -> Maybe Reset
encodeDfuD :: BitVector 64 -> Maybe Float
encodeCfuI :: BitVector 68 -> Maybe Instr
