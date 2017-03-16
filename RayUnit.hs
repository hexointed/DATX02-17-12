{-# LANGUAGE MultiWayIf #-}
module RayUnit where

import Base
import Float
import Vector
import DistFunc
import DFU

data RayUnit = RayUnit
	{ dfu :: DFU
	, pos :: Position
	, mdir :: Vector 3 Float
	, cache :: Vec 16 (Vec 16 FunOp)
	, ready :: Bool
	, funId :: FunId
	, index :: FunIndex
	}

initialize p dir = RayUnit {
		dfu = clean,
		pos = p,
		mdir = dir,
		cache = repeat (repeat $ Right (Val 0)),
		ready = True,
		funId = 0,
		index = 0
	}

calcRay :: RayUnit -> Maybe (Position, Vector 3 Float) -> (RayUnit, Maybe (FunId, Float))
calcRay r p = case ready r of
	True  -> case p of
		Just (p, d)  -> ((initialize p d) {ready = False}, Nothing)
		Nothing      -> (ray', res')
	False -> undefined
	where
		(ray', res') = stepR r

stepR :: RayUnit -> (RayUnit, Maybe (FunId, Float))
stepR r = undefined
	where
		(dfu', result') = step (dfu r) (reset, pos r)
		reset = if
			| funId r >= 16 -> Done
			| index r >= 16 -> Next $ funId r + 1
			| otherwise     -> Continue $ cache r !! funId r !! index r
