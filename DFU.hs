{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module DFU (DFU, Reset(..), Result(..), clean, step) where

import DistFunc
import Float
import Base
import Stack

data DFU = DFU
	{ minValue :: Float
	, minId :: FunId
	, stack :: Stack Float
	, index :: FunIndex
	, funId :: FunId
	}
	deriving (Eq, Show, Generic, NFData)

data Reset 
	= Continue FunOp
	| Done
	| Next FunId
	deriving (Eq, Show, Generic, NFData)

data Result
	= Result FunId Float
	| MemAccess FunId FunIndex
	| NoResult
	deriving (Eq, Show, Generic, NFData)

clean :: DFU
clean = DFU maxBound 0 (push maxBound empty) 0 0

step :: DFU -> (Reset, Position) -> (DFU, Result)
step scene (r,p) = case r of
	Continue op -> (stepOp scene p op, memAccess scene)
	Next id     -> (reset scene id, NoResult)
	Done        -> (clean, result $ reset scene 0)

reset :: DFU -> FunId -> DFU
reset s id = s {
		minValue = fst min',
		minId = snd min',
		index = 0,
		funId = id
	}
	where
		min' = minWith fst current next
		current = (minValue s, minId s)
		next = (top (stack s), funId s)

memAccess = liftf MemAccess funId index
result = liftf Result minId minValue

stepOp :: DFU -> Position -> FunOp -> DFU
stepOp scene p op = scene {
		stack = either pushOp (push . lookUp p) op (stack scene),
		index = index scene + 1
	}

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)
