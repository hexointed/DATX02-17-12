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

fetch :: Ptr DIMem -> Ptr DDMem -> Ptr CIMem -> (Maybe Reset, Maybe Float, Maybe Instr) 
fetch dfuip dfudp cfuip = (dfuIMemory dfuip, dfuDMemory dfudp, cfuIMemory cfuip)

encodeDfuI :: BitVector 68 -> Maybe Reset
encodeDfuI b = Just $ undefined

encodeDfuD :: BitVector 64 -> Maybe Float
encodeDfuD b = Just $ bitCoerce b

encodeCfuI :: BitVector 16 -> Maybe Instr
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
		
		bcho =             bSlice d0  d2 b
		cptr = bitCoerce $ bSlice d2  d4 b
		bact =             bSlice d6  d2 b
		pptr = bitCoerce $ bSlice d8  d3 b
		sptr = bitCoerce $ bSlice d12 d4 b
