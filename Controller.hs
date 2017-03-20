{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Controller where

import Base
import Float
import Vector
import RayUnit
import DistFunc
import RayUnitQC
import qualified Prelude as P
import qualified Control.Concurrent as C

data Pixel
	= R
	| I
	| O
	deriving (Eq, Generic, NFData)

type FrameBuffer = Vec (64*20) Pixel

data Controller = Controller
	{ frameBuffer :: FrameBuffer,
	  pixel :: FunIndex
	}

runSim = sequence $
	concatMap (P.take 1) . P.iterate (P.drop 500) $
	fmap (putStrLn . showBuffer) $
	sample controller

controller = fmap snd controllerSt

incI c mff = fmap ((,) (pos' c, position 0 0 1)) $ fmap frameBuffer $ case mff of
	Nothing         -> dup c
	Just (fid, val) -> dup (update val c)
update val c = c {
		frameBuffer = replace (pixel c) color (frameBuffer c),
		pixel = if pixel c < 64*20-1 then pixel c + 1 else 0
	}
	where 
		newPos = pos' c
		color
			| val < epsilon = R
			| otherwise     = O
pos' c = fmap (fromInteger . toInteger) $ position x y 0
	where
		x = rem (pixel c + 1) 64
		y = div (pixel c + 1) 64

controllerSt :: Signal ((Position, Vector 3 Float), FrameBuffer)
controllerSt = mealy incI (Controller (repeat I) 0) (register Nothing rayUnit)

rayUnit = mealy stepR (initialize origin origin testFuncs) (fmap fst controllerSt)

showBuffer b = unlines $ foldr (:) [] $ fmap (foldr (P.++) "") $ unconcat d64 (fmap show b)

instance Show Pixel where
	show R = "\x2588"
	show I = "?"
	show O = " "
