module CFU where

import Base
import Float
import DFU
import Stack

data Choice
	= NZ
	| Z
	deriving Eq

data Cond
	= Cond Choice StackPtr

type PackPtr = Unsigned 4
type StackPtr = Unsigned 4

data Action
	= PushQ
	| PushF
	| Drop
	| SetVal PackPtr StackPtr

data Instr
	= Instr Cond Action

--------------------------------------------------------------------------------

type IPtr = Unsigned 8
type Pack = Vec 8 Float

data CFU = CFU IPtr Pack
data Output
	= Q Pack
	| F Pack
	| Ready
	| Wait

step :: CFU -> (Stack Float, Maybe Instr) -> (CFU, Output)
step (CFU iptr pack) (st, inst) = case inst of
	Nothing          -> (CFU iptr pack, Ready)
	Just (Instr c a) -> case checkCond c st of
		True  -> trySend a $ CFU nexti (upack a pack st)
		False -> (CFU nexti pack, Wait)
		where
			nexti = iptr + 1

upack a p s = case a of
	SetVal pptr sptr -> replace pptr (topN sptr s) p
	_                -> p

trySend a (CFU iptr pack) = case a of
	PushF -> (CFU iptr pack, F pack)
	PushQ -> (CFU iptr pack, Q pack)
	_     -> (CFU iptr pack, Wait)

checkCond (Cond ch ptr) stack = f (topN ptr stack)
	where f 
		| ch == NZ = (/= 0)
		| ch ==  Z = (== 0)
