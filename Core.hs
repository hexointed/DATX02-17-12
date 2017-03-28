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
	, packOut :: Pack
	, packType :: PackType
	, ready :: Bool
	}

data PackType = Frame | Queue | None

initial' :: Core
initial' = undefined

ready' :: CoreOut
ready' = undefined

step' core input = case cfuS of
	Ready -> case nextPack input of
		Nothing -> (core, ready')
		Just p  -> (initial', ready')
	where
		(core', dfuS, cfuS) = step'' core undefined undefined

step'' core rpn instr = (core', compResult, output)
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

