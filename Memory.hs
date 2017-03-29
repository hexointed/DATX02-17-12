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
dfuIMemory = encodeDfuI . asyncRomFile d512 "dfuIMemory.bin"

dfuDMemory :: Ptr DDMem -> Maybe Float
dfuDMemory = encodeDfuD . asyncRomFile d512 "dfuDMemory.bin"

cfuIMemory :: Ptr CIMem -> Maybe Instr
cfuIMemory = encodeCfuI . asyncRomFile d512 "cfuIMemory.bin"

fetch :: CoreOut -> CoreIn 
fetch co = CoreIn { nextPack=Nothing
           , dfuInstr=dfuIMemory (dfuIPtr co)
           , dfuData=dfuDMemory (dfuDPtr co)
           , cfuInstr=cfuIMemory (cfuIPtr co)
           }

encodeDfuI :: BitVector 68 -> Maybe Reset
encodeDfuI b = Just $ undefined

encodeDfuD :: BitVector 64 -> Maybe Float
encodeDfuD b = Just $ bitCoerce b

encodeCfuI :: BitVector 68 -> Maybe Instr
encodeCfuI b = Just $ Instr cond action
	where
		cond = Cond choice cptr
		choice = case bcho of
			0 -> NZ
			1 -> Z
			2 -> A
			3 -> error "invalid choice"
		action = case bact of
			0 -> PushF
			1 -> Drop
			2 -> error "invalid action"
			3 -> SetVal pptr sptr
		
		bcho = bTake d2 b
		cptr = bTake d4 (bDrop d2 b)
		bact = btake d2 (bDrop d6 b)
		pptr = 
		sptr =
