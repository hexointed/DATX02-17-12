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
	, st :: CoreState
	}
	deriving (Eq, Show, Generic, NFData)

data CoreState
	= Working
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
					idptr = resize $ bitCoerce $ shiftR (last p) 0,
					icptr = resize $ bitCoerce $ shiftR (last p) 8,
					ddptr = resize $ bitCoerce $ shiftR (last p) 16,
					st = Working
				}
	Working -> case cfuS of
		Ready    -> case dfuS of
			Ready -> (initial', output initial')
			_     -> (core', output core')
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
		icptr' = case (output,instr) of
			(Ready, _)   -> icptr
			(_, Nothing) -> icptr
			_            -> icptr + 1

