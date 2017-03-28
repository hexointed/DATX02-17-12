{-# LANGUAGE UndecidableInstances #-}

module CFU (CFU, Out(..), Instr) where

import Base
import Float
import Stack
import Pack
import Stateful
import Indexed
import DFU

data Choice
	= NZ
	| Z
	| A
	deriving Eq

data Cond
	= Cond Choice (Ptr (Stack Float))

data Action
	= PushF
	| Drop
	| SetVal (Ptr Pack) (Ptr (Stack Float))

data Instr
	= Instr Cond Action

instance Indexed Instr where
	type Size Instr = 8

--------------------------------------------------------------------------------

data CFU = CFU 
	{ pack :: Pack 
	, stack :: Stack Float
	, st :: CFUState
	}

data CFUState
	= Waiting
	| Working

instance Stateful CFU where
	type In CFU = (Output DFU, Maybe Instr)
	type Out CFU = Pack

	step cfu (dfuOut, inst) = case st cfu of
		Working -> case inst of
			Nothing          -> (cfu, WaitI)
			Just (Instr c a) -> execInst c a cfu
		Waiting -> case dfuOut of
			WaitI      -> (cfu, Ready)
			Ready      -> (cfu, Ready)
			Result sta -> (cfu { st = Working, stack = sta }, WaitI)
	
	initial = CFU (repeat undefined) empty Waiting

execInst c a cfu = case checkCond c (stack cfu) of
	False -> (cfu, WaitI)
	True  -> case a of
		PushF      -> (cfu, Result $ pack cfu)
		Drop       -> (cfu { st = Waiting }, Ready)
		SetVal p s -> 
			(cfu { pack = replace p (topN s (stack cfu)) (pack cfu) }, WaitI)

checkCond (Cond ch ptr) stack = f (topN ptr stack)
	where f 
		| ch == NZ = (/= 0)
		| ch ==  Z = (== 0)
		| ch ==  A = (const True)
