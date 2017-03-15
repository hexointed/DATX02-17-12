{-# LANGUAGE DeriveGeneric #-}

module SceneUnit where

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
	, currentId :: FunId
	, position :: Position
	}
	deriving (Eq, Show, Generic)

data Reset 
	= Reset Position FunId
	| Continue FunOp
	deriving (Eq, Show, Generic)

instance NFData SceneUnit
instance NFData Reset

init :: SceneUnit
init = SceneUnit maxBound 0 (filled maxBound) 0 0 origin

stepOp :: SceneUnit -> FunOp -> SceneUnit
stepOp scene op = scene {
		stack = pushFunOp op (position scene) (stack scene),
		index = index scene + 1
	}

reset :: SceneUnit -> Position -> FunId -> SceneUnit
reset s p id = s {
		minValue = minValue',
		minId = minId',
		stack = filled maxBound,
		index = 0,
		currentId = id,
		position = p
	}
	where
		(minValue', minId') = minWith fst current next
		next = (minValue s, minId s)
		current = (top (stack s), currentId s)

type MemAccess = (FunId, FunIndex)

step :: SceneUnit -> Reset -> (SceneUnit, Either MemAccess Float)
step scene r = case r of
	Continue op -> (stepOp scene op, Left (currentId scene, index scene))
	Reset p id  -> (reset scene p id, Right $ top $ stack scene)

pushOp :: Op -> Stack Float -> Stack Float
pushOp op s = push newValue (popN (arity op) s)
	where
		newValue = apply op (topN 0 s) (topN 1 s)

pushFunOp :: FunOp -> Position -> Stack Float -> Stack Float
pushFunOp (Left op) p = pushOp op
pushFunOp (Right d) p = (push $ lookUp p d)
