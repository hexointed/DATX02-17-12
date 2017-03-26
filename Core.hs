module Core where

import Float
import Base
import Indexed

import Queue
import Pack
import CFU
import DFU

type ICache = Vec 16 Instr
type FCache = Vec 16 (Input DFU)

data Core = Core
	{ cfu :: CFU
	, dfu :: DFU
	, queue :: Queue 4 Pack
	, icache :: ICache 
	, iptr :: Ptr ICache
	, fcache :: FCache
	, fptr :: Ptr FCache
	}

type Pixel = (Float, Float)
type Color = (Float, Float, Float)

step :: Core -> Maybe Pixel -> (Core, Maybe Color)
step core px = undefined

step' core = core {
		cfu = undefined,
		dfu = undefined
	}
