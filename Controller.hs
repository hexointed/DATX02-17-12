{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Controller where

import Base
import Float
import Vector
import RayUnit
import DistFunc
import RayUnitQC

data Pixel
	= R
	| G
	| B
	deriving (Eq, Show, Generic, NFData)

type FrameBuffer = Vec (4*4) Pixel

data Controller = Controller
	{ frameBuffer :: FrameBuffer,
	  pixel :: FunIndex
	}

controller = fmap snd controllerSt

incI c mff = fmap ((,) (pos' c, position 0 0 1)) $ fmap frameBuffer $ case mff of
	Nothing         -> dup c
	Just (fid, val) -> dup (update val c)
update val c = c {
		frameBuffer = replace (pixel c) R (frameBuffer c),
		pixel = if pixel c < 15 then pixel c + 1 else 0
	}
	where color
		| val < epsilon = R
		| otherwise     = B
pos' c = fmap (fromInteger . toInteger) $ position (rem (pixel c) 4) (div (pixel c) 4) 0

controllerSt :: Signal ((Position, Vector 3 Float), FrameBuffer)
controllerSt = mealy incI (Controller (repeat G) 0) (register Nothing rayUnit)

rayUnit = mealy stepR (initialize origin origin testFuncs) (fmap fst controllerSt)
