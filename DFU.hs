module DFU where

import DistFunc
import Float
import Base

data DFU = DFU
	{ stack :: Vec 15 Float
	, index :: Unsigned 4
	, position :: Position
	}

clean :: DFU
clean = DFU (repeat 0) 0 origin

pushValue :: DFU -> Float -> DFU
pushValue dfu v = dfu {
		stack = replace (index dfu) v (stack dfu),
		index = index dfu + 1
	}

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
			newValue = apply op (stack' !! newPos) (stack' !! newPos + 1)
			newPos = index dfu - arity op + 1

pushFunOp :: DFU -> FunOp -> DFU
pushFunOp dfu (Fun op)    = pushOp dfu op
pushFunOp dfu (FunData d) = pushData dfu d

exec :: DFU -> Either FunOp Position -> (DFU, Maybe Float)
exec dfu op = case op of
	Left op -> (dfu' op, Nothing)
	Right p -> (clean{position=p}, Just (stack dfu !! index dfu))
	where 
		dfu' op = pushFunOp dfu op
