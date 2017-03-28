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

type CIMem = Vec 16 Instr
type DIMem = Vec 16 Reset
type DDMem = Vec 16 Float

data Core = Core
	{ cfu :: CFU
	, dfu :: DFU
	, icptr :: Ptr CIMem
	, idptr :: Ptr DIMem
	, ddptr :: Ptr DDMem
	, pack :: Pack
	}

data CoreIn = CoreIn
	{ nextPack :: Maybe Pack
	, dfuInstr :: Reset
	, dfuData :: Float
	, cfuInstr :: Instr
	}

data CoreOut = CoreOut
	{ dfuIPtr :: Ptr DIMem
	, dfuDPtr :: Ptr DDMem
	, cfuIPtr :: Ptr CIMem
	, frameOut :: Pack
	, queueOut :: Pack
	, ready :: Bool
	}

instance Stateful Core where
	type In Core = Pack
	type Out Core = Out CFU

	initial = undefined

	step core p = case cfuS of
		Ready -> (core' { pack = p }, Ready)
		
		where
			(core', dfuS, cfuS) = step' core undefined undefined


step' core rpn instr = (core', compResult, output)
	where
		core' = core 
			{ cfu = cfu'
			, dfu = dfu'
			, icptr = icptr' core
			, idptr = idptr' core
			}
		(dfu', compResult) = step (dfu core) (rpn, pack core)
		(cfu', output) = step (cfu core) (compResult, Just instr)
		idptr' = case compResult of
			WaitI -> idptr + 1
			_     -> idptr
		icptr' = case output of
			WaitI -> icptr + 1
			_     -> icptr

