{-# LANGUAGE DeriveGeneric #-}

module SceneUnit where

import GHC.Generics (Generic)
import Control.DeepSeq
import DistFunc
import Float
import Base
import DFU

data SceneUnit = SceneUnit
	{ minValue :: Float
	, minId :: FunId
	, dfu :: DFU
	, index :: FunIndex
	, currentId :: FunId
	}
	deriving (Eq, Show, Generic)

data Reset 
	= Reset Position FunId
	| Continue FunOp
	deriving (Eq, Show, Generic)

instance NFData SceneUnit
instance NFData Reset

init :: SceneUnit
init = SceneUnit maxBound 0 (clean origin) 0 0

stepOp :: SceneUnit -> FunOp -> SceneUnit
stepOp scene op = scene {
		dfu = pushFunOp (dfu scene) op,
		index = index scene + 1
	}

reset :: SceneUnit -> Position -> FunId -> SceneUnit
reset s p id = s {
		minValue = minValue',
		minId = minId',
		dfu = clean p,
		currentId = id
	}
	where
		(minValue', minId') = minWith (minValue s, minId s) (currentValue (dfu s), currentId s)

step :: SceneUnit -> Reset -> (SceneUnit, Either FunIndex Float)
step scene r = case r of
	Continue op -> (stepOp scene op, Left $ index scene)
	Reset p id  -> (reset scene p id, Right $ currentValue $ dfu scene)
