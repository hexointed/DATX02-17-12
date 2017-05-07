{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Core where

import Float
import Base
import Indexed
import Pack
import DFU
import Data.Maybe
import DistFunc

type DIMem = Vec 256 Instr
type DDMem = Vec 256 Float

data Core = Core
	{ dfu :: DFU
	, idptr :: Ptr DIMem
	, stall :: Bool
	}
	deriving (Eq, Show, Generic, NFData)

data CoreIn = CoreIn
	{ nextPack :: Maybe Pack
	, queueAck :: Bool
	, dfuInstr :: Maybe Instr
	, dfuData :: Maybe Float
	}
	deriving (Eq, Show, Generic, NFData)

data CoreOut = CoreOut
	{ dfuIPtr :: Ptr DIMem
	, dfuDPtr :: Ptr DDMem
	, packOut :: Pack
	, packType :: PackType
	, wantPack :: Bool
	}
	deriving (Eq, Show, Generic, NFData)

initial' :: Core
initial' = Core initial 0 False

output :: Core -> (Core, CoreOut)
output c = (,) c $ CoreOut
	{ dfuIPtr = idptr c
	, dfuDPtr = 0
	, packOut = pack (dfu c)
	, packType = None
	, wantPack = ready (dfu c)
	}

step' core input = case ready (dfu core) of
	True -> case nextPack input of
		Nothing       -> output core'
		Just p        -> output start
			where start = core' {
					dfu = (dfu core') { pack = p, ready = False },
					idptr = resize $ bitCoerce (head p)
				}
	False -> case dfuS of
		Left ptr -> case ptr of
			Just ptr -> fmap (setPtr ptr) $ case stall core of
				False -> output $ core  { stall = True }
				True  -> output $ core' { stall = False }
			Nothing  -> output core'
		Right pt -> case queueAck input of
			False -> case pt of
				None -> fmap (\x -> x { packType = pt }) (output core')
				_    -> fmap (\x -> x { packType = pt }) (output core)
			True  -> (output core')
	where
		(core', dfuS) = step'' core nextInstruction d 
		d = fromMaybe 0 (dfuData input)
		setPtr ptr x = x { dfuDPtr = ptr }
		nextInstruction
			| ready (dfu core) = Just $ Comp (Oper Nop)
			| otherwise        = dfuInstr input

step'' core instr global = case instr of
	Nothing    -> (core, Left Nothing)
	Just instr -> (core { dfu = dfu', idptr = idptr' }, output)
		where
			(dfu', output) = step (dfu core) (instr, global)
			idptr' = idptr core + 1
