{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Controller where

import Base
import Float
import Vector
import RayUnit
import DistFunc
import RayUnitQC
import qualified Prelude as P

data Pixel
	= R
	| G
	| B
	deriving (Eq, Show, Generic, NFData)

type FrameBuffer = Vec (8*8) Pixel

data Controller = Controller
	{ frameBuffer :: FrameBuffer,
	  pixel :: FunIndex
	}

topEntity = controller

controller = fmap snd controllerSt

incI c mff = fmap ((,) (pos' c, position 0 0 1)) $ fmap frameBuffer $ case mff of
	Nothing         -> dup c
	Just (fid, val) -> dup (update val c)
update val c = c {
		frameBuffer = replace (pixel c) color (frameBuffer c),
		pixel = if pixel c < 8*8-1 then pixel c + 1 else 0
	}
	where 
		newPos = pos' c
		color
			| val < epsilon = R
			| otherwise     = B
pos' c = fmap (fromInteger . toInteger) $ position x y 0
	where
		x = rem (pixel c + 1) 8
		y = div (pixel c + 1) 8

controllerSt :: Signal ((Position, Vector 3 Float), FrameBuffer)
controllerSt = mealy incI (Controller (repeat G) 0) (register Nothing rayUnit)

rayUnit = mealy stepR (initialize origin origin testFuncs) (fmap fst controllerSt)

showBuffer b = unlines $ foldr (:) [] $ fmap (foldr (P.++) "") $ unconcat d8 (fmap show b)
