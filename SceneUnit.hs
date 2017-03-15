{-# LANGUAGE DeriveGeneric #-}

module SceneUnit (SceneUnit, Reset(..), Result(..), clean, step) where

import GHC.Generics (Generic)
import Control.DeepSeq
import DistFunc
import Float
import Base
import Stack

data SceneUnit = SceneUnit
	{ minValue :: Float
	, minId :: FunId
	, stack :: Stack Float
	, index :: FunIndex
	, funId :: FunId
	}
	deriving (Eq, Show, Generic)

data Reset 
	= Continue FunOp
	| Done
	| Next FunId
	deriving (Eq, Show, Generic)

data Result
	= Result FunId Float
	| MemAccess FunId FunIndex
	| NoResult
	deriving (Eq, Show, Generic)

instance NFData SceneUnit
instance NFData Reset
instance NFData Result

clean :: SceneUnit
clean = SceneUnit maxBound 0 (filled maxBound) 0 0

stepOp :: SceneUnit -> Position -> FunOp -> SceneUnit
stepOp scene pos op = scene {
		stack = pushFunOp op pos (stack scene),
		index = index scene + 1
	}

reset :: SceneUnit -> FunId -> SceneUnit
reset s id = s {
		minValue = minValue',
		minId = minId',
		stack = filled maxBound,
		index = 0,
		funId = id
	}
	where
		(minValue', minId') = minWith fst current next
		current = (minValue s, minId s)
		next = (top (stack s), funId s)

step :: SceneUnit -> (Reset, Position) -> (SceneUnit, Result)
step scene (r,p) = case r of
	Continue op -> (stepOp scene p op, memAccess scene)
	Next id -> (reset scene id, NoResult)
	Done -> (clean, result scene)

memAccess = liftf MemAccess funId index
result= liftf Result minId minValue

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 0 s) (topN 1 s)

pushFunOp :: FunOp -> Position -> Stack Float -> Stack Float
pushFunOp (Left op) p = pushOp op
pushFunOp (Right d) p = (push $ lookUp p d)
