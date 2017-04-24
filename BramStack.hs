module BramStack (bramStack, QIndex) where

import Base
import Pack
import Float

type QSize = 64
type QIndex = Unsigned (CLog 2 QSize + 1)

bramStack :: Signal (Bool, Bool) -> Signal Pack -> Signal (QIndex, Maybe Pack)
bramStack pp pack = 
	(fmap (\p i -> if i == -1 then (0, Nothing) else (i+1, Just p)) $
	setFirst (repeat 0) $ 
	readNew (blockRam (repeat (repeat 0) :: Vec QSize Pack))
		(fmap nextIndex i <*> pp)
		push''
	) <*> register (-1) i
	where
		i = (index pp)

		push'' :: Signal (Maybe (QIndex, Pack))
		push'' = fmap push' pack <*> i <*> pp

push' pack i pp = case fst pp of
	True  -> Just (i + 1, pack)
	False -> Nothing

index = mealy index' 0

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

nextIndex i (push, pop)
	| i >= 64   = 0
	| otherwise = case pop of
		True -> case push of
			True  -> i
			False -> case i == 0 of
				True  -> i
				False -> i - 1
		False -> case push of
			True  -> i + 1
			False -> i
