module Memory where

import Core
import Base
import Indexed
import Pack
import DFU
import DistFunc
import Float

dfuIMemory :: Ptr DIMem -> Maybe Instr
dfuIMemory = encodeDfuI . asyncRomFile d512 "dfuIMemory.bin"

dfuDMemory :: Ptr DDMem -> Maybe Float
dfuDMemory = encodeDfuD . asyncRomFile d512 "dfuDMemory.bin"

fetch :: () -> (Ptr DIMem, Ptr DDMem) -> ((), (Maybe Instr, Maybe Float)) 
fetch () (di, dd) = (,) () (dfuIMemory di, dfuDMemory dd)

encodeDfuI :: BitVector 18 -> Maybe Instr
encodeDfuI b = Just stage
	where
		stage = case bsta of
			0 -> Instr cond action
			1 -> Comp funop
			2 -> Next funid
		funop = case bfop of
			0 -> Left op
			1 -> Right datae
		datae = case bop of
			0 -> Point bflo
			1 -> Arg bpac
		op = case bdat of
			0 -> Max
			1 -> Min
			2 -> Add
			3 -> Sub
			4 -> Mul
			5 -> Div
			6 -> Sqrt
			7 -> Abs
			8 -> Floor
		funid = bfid
		cond = Cond choice cptr
		choice = case bcho of
			0 -> NZ
			1 -> Z
			2 -> A
			3 -> error "invalid choice"
		action = case bact of
			0 -> PushF
			1 -> PushQ
			2 -> Drop
			3 -> SetVal pptr sptr

		bsta =             bSlice d0  d2 b
		bfop =             bSlice d2  d1 b
		bop  =             bSlice d3  d1 b
		bflo = bitCoerce $ bSlice d4  d8 b
		bpac = bitCoerce $ bSlice d4  d3 b
		bdat =             bSlice d3  d4 b
		bfid = bitCoerce $ bSlice d2  d16 b
		bcho =             bSlice d2  d2 b
		cptr = bitCoerce $ bSlice d4  d4 b
		bact =             bSlice d8  d2 b
		pptr = bitCoerce $ bSlice d10 d3 b
		sptr = bitCoerce $ bSlice d14 d4 b

encodeDfuD :: BitVector (BitSize Float) -> Maybe Float
encodeDfuD b = Just $ bitCoerce b
