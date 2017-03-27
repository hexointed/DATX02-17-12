{-# LANGUAGE UndecidableInstances #-}

module Core where

import Float
import Base
import Indexed

import Queue
import Pack
import CFU
import DFU
import Stateful

type ICache = Vec 16 Instr
type FCache = Vec 16 Reset

data Core = Core
	{ cfu :: CFU
	, dfu :: DFU
	, icache :: ICache 
	, iptr :: Ptr ICache
	, fcache :: FCache
	, fptr :: Ptr FCache
	, pack :: Pack
	}

instance Stateful Core where
	type In Core = Pack
	type Out Core = Out CFU

	initial = undefined

	step core p = case cfuS of
		Ready -> core' { pack = p }
		
		where
			(core', dfuS, cfuS) = step' core


step' core = (core', compResult, output)
	where
		core' = core 
			{ cfu = cfu'
			, dfu = dfu'
			, iptr = iptr' core
			, fptr = fptr' core
			}
		(dfu', compResult) = step (dfu core) (rpn, pack core)
		(cfu', output) = step (cfu core) (compResult, Just instr)
		rpn = fcache core !! fptr core
		instr = icache core !! iptr core
		fptr' = case compResult of
			WaitI -> fptr + 1
			_     -> fptr
		iptr' = case output of
			WaitI -> iptr + 1
			_     -> iptr

