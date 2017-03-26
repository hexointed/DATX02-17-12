module CFU (CFU, module Stateful, Instr) where

import Base
import Float
import DFU
import Stack
import Pack
import Stateful
import Indexed

data Choice
	= NZ
	| Z
	deriving Eq

data Cond
	= Cond Choice (Ptr (Stack Float))

data Action
	= PushQ
	| PushF
	| Drop
	| SetVal (Ptr Pack) (Ptr (Stack Float))

data Instr
	= Instr Cond Action

instance Indexed Instr where
	type Size Instr = 8

--------------------------------------------------------------------------------

data CFU = CFU (Ptr Instr) Pack
data Out
	= Q Pack
	| F Pack
	| Ready
	| Wait

instance Stateful CFU where
	type Input CFU = (Stack Float, Maybe Instr)
	type Output CFU = Out

	step (CFU iptr pack) (st, inst) = case inst of
		Nothing          -> (CFU iptr pack, Ready)
		Just (Instr c a) -> case checkCond c st of
			True  -> trySend a $ CFU nexti (upack a pack st)
			False -> (CFU nexti pack, Wait)
			where
				nexti = iptr + 1
	
	initial = CFU 0 (repeat undefined)

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
