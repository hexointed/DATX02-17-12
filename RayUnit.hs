{-# LANGUAGE MultiWayIf #-}
module RayUnit where

import Base
import Float
import Vector
import DistFunc
import DFU

epsilon = 0.001

data RayUnit = RayUnit
	{ dfu :: DFU
	, pos :: Position
	, mdir :: Vector 3 Float
	, cache :: Vec 128 (Maybe FunOp)
	, ready :: Bool
	, funId :: FunId
	, index :: FunIndex
	, steps :: Unsigned 8
	}

initialize p dir funcs = RayUnit {
		dfu = clean,
		pos = p,
		mdir = dir,
		cache = funcs,
		ready = True,
		funId = 0,
		index = 0,
		steps = 0
	}

stepR :: RayUnit -> (Position, Vector 3 Float) -> (RayUnit, Maybe (FunId, Float))
stepR r (p,dir) = case result' of
	Just (fun, val) -> if
		| val <= epsilon || steps r == 5 -> (initialize p dir (cache r), Just (fun, val))
		| otherwise -> flip (,) Nothing $ r {
				dfu = clean,
				pos = pos r .+ scale val (mdir r),
				index = 0,
				funId = 0,
				steps = steps r + 1
			}
	Nothing         -> flip (,) Nothing $ r {
				dfu = dfu',
				index = index r + 1,
				funId = funId r + nf
			}
	where
		(dfu', result') = step (dfu r) (reset, pos r)
		(reset, nf) = case cache r !! index r of
			Just op ->            (Continue op, 0)
			Nothing -> if
				| index r >= 127 -> (Done, 0)
				| otherwise      -> (Next (funId r + 1), 1)
