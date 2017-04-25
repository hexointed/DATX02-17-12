module Memory where

import Core
import Base
import Indexed
import DFU
import DistFunc
import Float

dfuIMemory :: Ptr DIMem -> Maybe Instr
dfuIMemory = encodeDfuI . asyncRomFile d512 "dfuIMemory.bin"

dfuDMemory :: Ptr DDMem -> Maybe Float
dfuDMemory = encodeDfuD . asyncRomFile d512 "dfuDMemory.bin"

fetch :: () -> (Ptr DIMem, Ptr DDMem) -> ((), (Maybe Instr, Maybe Float)) 
fetch () (di, dd) = (,) () (dfuIMemory di, dfuDMemory dd)

encodeDfuI :: BitVector 16 -> Maybe Instr
encodeDfuI b = Just instr
	where
		instr = case sel of
			0xC -> Comp $ Arg arg1
			0xD -> Comp $ Point argn
			0xE -> Next . resize $ aptr
			0xF -> undefined
			_   -> case hsel of
				0x0 -> Instr (Cond condition (resize cptr)) action
				0x1 -> Comp op
		op = case opcd of
			0x00 -> Oper Max
			0x01 -> Oper Min
			0x02 -> Oper Add
			0x03 -> Oper Sub
			0x04 -> Oper Mul
			0x05 -> Oper Div
			0x06 -> Oper Sqrt
			0x07 -> Oper Abs
			0x08 -> Oper Floor
			0x09 -> Acc
			0x0a -> Oper Dot
			0x0b -> Oper Cross
                        0x0c -> Oper Addv
                        0x0d -> Oper Subv
                        0x0e -> Oper Scale
                        0x0f -> Oper Copy
                        0x10 -> Oper Norm
		condition = case opcc of
			0x0 -> A
			0x1 -> Z
			0x2 -> NZ
			0x3 -> error "Invalid condition"
		action = case opca of
			0x0 -> PushF
			0x1 -> PushQ
			0x2 -> Drop
			0x3 -> SetVal arg2 arg1
		
		arg1 = bitCoerce $ bSlice d12 d4  b
		arg2 = bitCoerce $ bSlice d8  d4  b
		argn = bitCoerce $ bSlice d8  d8  b
		aptr = bitCoerce $ bSlice d4  d12 b
		cptr = bitCoerce $ bSlice d1  d3  b

		opcd = bSlice d2  d6  b
		opcc = bSlice d4  d2  b
		opca = bSlice d6  d2  b

		sel  = bSlice d0  d4  b
		hsel = bSlice d0  d1  b


encodeDfuD :: BitVector (BitSize Float) -> Maybe Float
encodeDfuD = Just . bitCoerce
