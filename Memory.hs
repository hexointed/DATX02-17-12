module Memory where

import Core

dfuIMemory = asyncRomFile d512 "dfuIMemory.bin"
dfuDMemory = asyncRomFile d512 "dfuDMemory.bin"
cfuIMemory = asyncRomFile d512 "cfuIMemory.bin"

fetch :: CoreOut -> CoreIn 
fetch CO = { nextPack=Nothing
           , dfuInstr=dfuIMemory (dfuIPtr CO)
           , dfuData=dfuDMemory (dfuDPtr CO)
           , cfuInstr=cfuIMemory (cfuIPtr CO)
           }
