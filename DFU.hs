{-# LANGUAGE DeriveGeneric, DeriveAnyClass, UndecidableInstances #-}

module DFU (DFU, Reset(..)) where

import DistFunc
import Float
import Base
import Stack
import Pack
import Stateful
import Indexed

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
	| Compute
	| Done
	deriving (Eq, Show, Generic, NFData)

instance Stateful DFU where
	type In DFU = (Reset, SPack, Pack)
	type Out DFU = (Stack Float, Either (Maybe (Ptr Pack)) (Maybe (Ptr SPack)))

	step scene (r,p, p1) = case r of
		Continue op -> (stepOp scene p op p1, WaitI)
		Next id     -> (reset scene id, WaitI)
		Compute     -> (reset scene 0, Result (stack (reset scene 0), Left Nothing))
		Done        -> (initial, Ready)
                        
                      
	
	initial = DFU maxBound 0 (push maxBound (filled 0)) 0

reset :: DFU -> FunId -> DFU
reset s id = (s {
		minValue = fst min',
		minId = snd min',
		funId = id
	})
	where
		min' = minWith fst current next
		current = (minValue s, minId s)
		next = (top (stack s), funId s)



getData :: FunOp -> Maybe Data
getData (Right a) = Just a
getData _       = Nothing

stepOp :: DFU -> SPack -> FunOp -> Pack -> DFU
stepOp scene globalstack op localstack = scene {
        stack = either pushOp (push . lookUp globalstack localstack) op (stack scene)
        }

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)


