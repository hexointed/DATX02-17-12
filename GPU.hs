module GPU where

import Base
import Core
import Pack
import Queue
import TopQueue
import Memory


frameBufferWidth = d16
frameBufferHeight = d16
frameBuffer = asyncRam (frameBufferWidth `mulSNat` frameBufferHeight)

coreOut = mealy step' initial' coreIn

coreIn = meld (register Nothing mealyQueue) (register (Nothing, Nothing, Nothing) mealyMemory)

mealyQueue = mealy serve (filled $ repeat 0) (fmap (\c -> (ready c, pack' c)) coreOut)
	where
		pack' c = case packType c of
			Core.Queue -> Just $ packOut c
			_          -> Nothing

mealyMemory = mealy fetch () $ 
	fmap (\c -> (dfuIPtr c, dfuDPtr c, cfuIPtr c)) coreOut

meld q m = fmap (curry3 . CoreIn) q <*> m
