{-# LANGUAGE DeriveGeneric #-}

module DFU (DFU, clean, pushFunOp, currentValue) where

import GHC.Generics (Generic)
import Control.DeepSeq
import DistFunc
import Float
import Base

data DFU = DFU
	{ stack :: Vec 16 Float
	, index :: Unsigned 4
	, position :: Position
	}
	deriving (Eq, Show, Generic)

instance NFData DFU

clean :: Position -> DFU
clean = DFU (repeat maxBound) (-1)

pushValue :: DFU -> Float -> DFU
pushValue dfu v = dfu {
		stack = replace index' v (stack dfu),
		index = index'
	}
	where
		index' = index dfu + 1

pushData :: DFU -> Data -> DFU
pushData dfu (Val v) = pushValue dfu v
pushData dfu X       = pushValue dfu (x $ position dfu)
pushData dfu Y       = pushValue dfu (y $ position dfu)
pushData dfu Z       = pushValue dfu (z $ position dfu)

pushOp :: DFU -> Op -> DFU
pushOp dfu op = dfu {
		stack = replace newPos newValue stack',
		index = newPos
	}
		where
			stack' = stack dfu
			index' = index dfu
			newPos = index' - arity op + 1
			newValue = apply op (stack' !! index') (stack' !! (index' - 1))

pushFunOp :: DFU -> FunOp -> DFU
pushFunOp dfu (Fun op)    = pushOp dfu op
pushFunOp dfu (FunData d) = pushData dfu d

currentValue :: DFU -> Float
currentValue dfu = stack dfu !! index dfu
