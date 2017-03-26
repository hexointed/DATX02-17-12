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
	| Done
	| Next FunId
	deriving (Eq, Show, Generic, NFData)

instance Stateful DFU where
	type Input DFU = (Reset, Pack)
	type Output DFU = Maybe (FunId, Float)

	step scene (r,p) = case r of
		Continue op -> (stepOp scene p op, Nothing)
		Next id     -> (reset scene id, Nothing)
		Done        -> (initial, result $ reset scene 0)
	
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

result = Just . liftf (,) minId minValue

stepOp :: DFU -> Pack -> FunOp -> DFU
stepOp scene p op = scene {
		stack = either pushOp (push . lookUp p) op (stack scene)
	}

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)
