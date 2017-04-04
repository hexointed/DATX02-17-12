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

fetch :: () -> (Ptr DIMem, Ptr DDMem, Ptr CIMem) -> 
		((), (Maybe Reset, Maybe Float, Maybe Instr)) 
fetch () (di, dd, ci) = (,) () (dfuIMemory di, dfuDMemory dd, cfuIMemory ci)

encodeDfuI :: BitVector (BitSize Float + 4) -> Maybe Reset
encodeDfuI b = Just stage
	where
		stage = case bsta of
			0 -> Continue funop
			1 -> Next funid
			2 -> Compute
			3 -> Done
		funop = case bfop of
			0 -> Left datae
			1 -> Right op
		op = case bop of
			0 -> Val bflo
			1 -> Arg bpac
		datae = case bdat of
			0 -> Max
			1 -> Min
			2 -> Add
			3 -> Sub
			4 -> Mul
			5 -> Div
			6 -> Sqrt
			7 -> Abs
		funid = bfid

		bsta =             bSlice d0  d2 b
		bfop =             bSlice d2  d1 b
		bop  =             bSlice d3  d1 b
		bflo = bitCoerce $ bSlice d4  d32 b
		bpac = bitCoerce $ bSlice d4  d3 b
		bdat =             bSlice d3  d3 b
		bfid = bitCoerce $ bSlice d0  d16 b

encodeDfuD :: BitVector (BitSize Float) -> Maybe Float
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
