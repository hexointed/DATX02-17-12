{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DFU where

import DistFunc
import Float
import Base
import Stack
import Pack
import Indexed

type FunId = Unsigned 16

data PackType = Frame | Queue | None
	deriving (Eq, Show, Generic, NFData)

data Choice
	= NZ
	| Z
	| A
	deriving (Eq, Show, Generic, NFData)

data Cond
	= Cond Choice (Ptr (Stack Float))
	deriving (Eq, Show, Generic, NFData)

data Action
	= PushF
	| PushQ
	| Drop
	| SetVal (Ptr Pack) (Ptr (Stack Float))
	deriving (Eq, Show, Generic, NFData)

data Instr
	= Instr Cond Action
	| Next FunId
	| Comp FunOp
	deriving (Eq, Show, Generic, NFData)

instance Indexed Instr where
	type Size Instr = 8

--------------------------------------------------------------------------------

data DFU = DFU
	{ minValue :: Float
	, minId :: FunId
	, stack :: Stack Float
	, funId :: FunId
	, pack :: Pack
	, ready :: Bool
	}
	deriving (Eq, Show, Generic, NFData)

initial = DFU maxBound 0 (push maxBound (filled 0)) 0 (repeat 0) True

step dfu inst = case inst of
		Comp r    -> (stepOp dfu (pack dfu) r, Left Nothing)
		Next i    -> (reset dfu i, Left Nothing)
		Instr c a -> execInst c a dfu

stepOp :: DFU -> Pack -> FunOp -> DFU
stepOp scene p op = scene {
		stack = either pushOp (push . lookUp p) op (stack scene)
	}

reset :: DFU -> FunId -> DFU
reset s id = s {
		minValue = fst min',
		minId = snd min',
		funId = id
	}
	where
		min' = minWith fst current next
		current = (minValue s, minId s)
		next = (top (stack s), funId s)

execInst c a dfu = case checkCond c (stack dfu) of
	False -> (dfu, Left Nothing)
	True  -> case a of
		PushF      -> (dfu, Right Frame)
		PushQ      -> (dfu, Right Queue)
		Drop       -> (dfu { ready = True }, Left Nothing)
		SetVal p s -> 
			(dfu { pack = replace p (topN s (stack dfu)) (pack dfu) }, Left Nothing)

checkCond (Cond ch ptr) stack = f (topN ptr stack)
	where f 
		| ch == NZ = (/= 0)
		| ch ==  Z = (== 0)
		| ch ==  A = (const True)

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)
