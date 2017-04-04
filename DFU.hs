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
	| Next FunId FunOp
	| Compute FunOp
	| Done
	deriving (Eq, Show, Generic, NFData)

instance Stateful DFU where
	type In DFU = (Reset, Pack, Pack)
	type Out DFU = (Stack Float, Maybe (Ptr Pack))

	step scene (r,p, p1) = case r of
		Continue op -> (stepOp scene p op p1, WaitI)
		Next id op   -> (d, WaitI)
                        where (d, b) = reset scene id op
		Compute op   -> (e, Result a)
                        where (e, a) = reset scene 0 op
		Done        -> (initial, Ready)
                        
                      
	
	initial = DFU maxBound 0 (push maxBound (filled 0)) 0

reset :: DFU -> FunId -> FunOp -> (DFU,(Stack Float, Maybe (Ptr Pack)))
reset s id op = (s {
		minValue = fst min',
		minId = snd min',
		funId = id
	}, (getStack s, getPointer op))
	where
		min' = minWith fst current next
		current = (minValue s, minId s)
		next = (top (stack s), funId s)

getStack :: DFU -> Stack Float
getStack (DFU _ _ stack _) = stack

getPointer :: FunOp -> Maybe (Ptr Pack)
getPointer (Right (Point a)) = Just a
getPointer (Right (Arg a))   = Just a
getPointer _              = Nothing

getData :: FunOp -> Maybe Data
getData (Right a) = Just a
getData _       = Nothing

stepOp :: DFU -> Pack -> FunOp -> Pack -> DFU
stepOp scene globalstack op localstack = scene {
        stack = either pushOp (push . lookUp globalstack localstack) op (stack scene)
        }

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 1 s) (topN 0 s)


