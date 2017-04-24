module BramStack (bramStack, QIndex) where

import Base
import Pack
import Float

type QSize = 64
type QIndex = Unsigned (CLog 2 QSize + 1)

bramStack :: Signal Bool -> Signal (Maybe Pack) -> Signal (QIndex, Maybe Pack)
bramStack pop push = 
	(fmap wantsPack pop' <*>) $
	(fmap hasPack stackIndex' <*>) $
	setFirst (repeat 0) $
	readNew (blockRam (repeat (repeat 0) :: Vec QSize Pack))
		readAddr
		writeAddr
	where
		stackIndex = index $ bundle (fmap (/= Nothing) push, pop)
		stackIndex' = register 0 stackIndex
		
		readAddr = fmap safeRead stackIndex
		writeAddr = fmap pushAddr stackIndex <*> push
		pop' = register False pop

wantsPack True (i, Just p) = (i, Just p)
wantsPack _    (i, _)      = (i, Nothing)

hasPack (-1) out = (-1, Nothing)
hasPack i out = (i, Just out)

pushAddr i p = do
	p <- p
	return (i + 1, p)

safeRead (-1) = 0
safeRead i    = i

index = mealy index' 0
	where
		index' :: QIndex -> (Bool, Bool) -> (QIndex, QIndex)
		index' i (push, pop) = case pop of
			True -> case push of
				True  -> dup i
				False -> case i of
					-1 -> dup i
					_  -> (i - 1, i)
			False -> case push of
				True  -> (i + 1, i)
				False -> dup i
