module Memory where

import Core

dfuIMemory :: Ptr DIMem -> Maybe Reset
dfuIMemory = encodeDfuI asyncRomFile d512 "dfuIMemory.bin"
dfuDMemory :: Ptr DDMem -> Maybe Float
dfuDMemory = encodeDfuD asyncRomFile d512 "dfuDMemory.bin"
cfuIMemory :: Ptr CIMem -> Maybe Instr
cfuIMemory = encodeCfuI asyncRomFile d512 "cfuIMemory.bin"

fetch :: CoreOut -> CoreIn 
fetch CO = CoreIn { nextPack=Nothing
           , dfuInstr=dfuIMemory (dfuIPtr CO)
           , dfuData=dfuDMemory (dfuDPtr CO)
           , cfuInstr=cfuIMemory (cfuIPtr CO)
           }

encodeDfuI :: BitVector 68 -> Maybe Reset
encodeDfuD :: BitVector 64 -> Maybe Float
encodeDfuI :: BitVector 68 -> Maybe Instr






