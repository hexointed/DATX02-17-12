{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DFU where

import DistFunc
import Float
import Base
import Stack
import Pack
import Indexed

type FunId = Unsigned 24

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

step dfu (inst, global) = case inst of
		Comp r    -> (stepOp dfu global r, Left $ dataAddr r)
		Next i    -> (reset dfu i, noAddr)
		Instr c a -> execInst c a dfu

stepOp :: DFU -> Float -> FunOp -> DFU
stepOp scene p op = scene {
		stack = flip ($) (stack scene) $ case op of
			Point _ -> push p
			Arg a   -> push (pack scene !! a)
			Oper op -> pushOp op
			Acc     -> push (minValue scene)
	}

reset :: DFU -> FunId -> DFU
reset scene id = (scene {
		minValue = fst min',
		minId = snd min',
		funId = id
	})
	where
		min' = minWith fst current next
		current = (minValue scene, minId scene)
		next = (top (stack scene), funId scene)

execInst c a dfu = case checkCond c (stack dfu) of
	False -> (dfu, noAddr)
	True  -> case a of
		PushF      -> (dfu, Right Frame)
		PushQ      -> (dfu, Right Queue)
		Drop       -> (initial, noAddr)
		SetVal p s -> 
			(dfu { pack = replace p (topN s (stack dfu)) (pack dfu) }, noAddr)

checkCond (Cond ch ptr) stack = f (topN ptr stack)
	where f 
		| ch == NZ = (/= 0)
		| ch ==  Z = (== 0)
		| ch ==  A = (const True)

pushOp :: Op -> Stack Float -> Stack Float
pushOp operation s = push newValue (popN (arity operation) s)
	where
		newValue = apply operation (topN 1 s) (topN 0 s)

dataAddr inst = case inst of
	(Point ptr) -> Just ptr
	_           -> Nothing

noAddr = Left Nothing
