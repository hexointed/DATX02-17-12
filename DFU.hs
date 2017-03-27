{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DFU (DFU, module Stateful, stack) where

import DistFunc
import Float
import Base
import Stack
import Pack
import Stateful

data DFU = DFU
	{ minValue :: Float
	, minId :: FunId
	, stack :: Stack Float
	, funId :: FunId
	}
	deriving (Eq, Show, Generic, NFData)

data Reset 
	= Continue FunOp
	| Next FunId
	| Result
	| Done
	deriving (Eq, Show, Generic, NFData)

instance Stateful DFU where
	type Input DFU = (Reset, Pack)
	type Output DFU = Either (FunId, Float) Ready

	step scene (r,p) = case r of
		Continue op -> (stepOp scene p op, Right Wait)
		Next id     -> (reset scene id, Right Wait)
		Result      -> (initial, Left . result $ reset scene 0)
		Done        -> (scene, Right Ready)
	
	initial = DFU maxBound 0 (push maxBound empty) 0

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

result = liftf (,) minId minValue

stepOp :: DFU -> Pack -> FunOp -> DFU
stepOp scene p op = scene {
		stack = either pushOp (push . lookUp p) op (stack scene)
	}

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)
