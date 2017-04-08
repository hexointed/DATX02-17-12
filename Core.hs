{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Core where

import Float
import Base
import Indexed

import Pack
import CFU
import DFU
import Stateful hiding (output)

type CIMem = Vec 256 Instr
type DIMem = Vec 256 Reset
type DDMem = Vec 256 Float

data Core = Core
	{ cfu :: CFU
	, dfu :: DFU
	, icptr :: Ptr CIMem
	, idptr :: Ptr DIMem
	, ddptr :: Ptr DDMem
	, pack :: Pack
	, st :: CoreState
	}
	deriving (Eq, Show, Generic, NFData)

data CoreState
	= WorkingD
	| WorkingC
	| Waiting
	deriving (Eq, Show, Generic, NFData)

data CoreIn = CoreIn
	{ nextPack :: Maybe Pack
	, dfuInstr :: Maybe Reset
	, dfuData :: Maybe Float
	, cfuInstr :: Maybe Instr
	}
	deriving (Eq, Show, Generic, NFData)

data CoreOut = CoreOut
	{ dfuIPtr :: Ptr DIMem
	, dfuDPtr :: Ptr DDMem
	, cfuIPtr :: Ptr CIMem
	, packOut :: Pack
	, packType :: PackType
	, ready :: Bool
	}
	deriving (Eq, Show, Generic, NFData)

initial' :: Core
initial' = Core initial initial 0 0 0 (repeat 0) Waiting

output :: Core -> CoreOut
output c = CoreOut
	{ dfuIPtr = idptr c
	, dfuDPtr = 0
	, cfuIPtr = icptr c
	, packOut = pack c
	, packType = None
	, ready = st c == Waiting
	}

step' core input = case st core of
	Waiting -> case nextPack input of
		Nothing -> (core, output core)
		Just p  -> (c', output c')
			where c' = initial' {
					pack = p, 
					idptr = resize $ bitCoerce $ shiftR (head p) 0,
					icptr = resize $ bitCoerce $ shiftR (head p) 8,
					ddptr = resize $ bitCoerce $ shiftR (head p) 16,
					st = WorkingD
				}
	WorkingD -> case dfuS of
		Result r -> let c' = core' {st = WorkingC} in (c', output c')
		_        -> (core', output core')
	WorkingC -> case cfuS of
		Ready    -> (initial', output initial')
		WaitI    -> (core', output core')
		Result p -> (core', (output core') {packOut = fst p, packType = snd p})
	where
		(core', dfuS, cfuS) = step'' core (dfuInstr input) (cfuInstr input)

step'' core rpn instr = (core', compResult, output)
	where
		core' = core 
			{ cfu = cfu'
			, dfu = dfu'
			, icptr = icptr' core
			, idptr = idptr' core
			}
		(dfu', compResult) = step (dfu core) (rpn, pack core)
		(cfu', output) = step (cfu core) (compResult, instr, pack core)
		idptr' = case (compResult, rpn) of
			(Ready, _)   -> idptr
			(_, Nothing) -> idptr
			_            -> idptr + 1
		icptr' = case (output,instr,compResult) of
			(Ready, _, _)    -> icptr
			(_, Nothing, _)  -> icptr
			(_, _, Result _) -> icptr
			_                -> icptr + 1

