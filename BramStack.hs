{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiWayIf #-}

module BramStack  where

import Base
import Pack
import Debug.Trace
import Float

type QSize = 32
type QIndex = Unsigned 5

data Push
	= Top Pack
	| Bottom Pack
	| None
	deriving (Eq, Generic, NFData, Show)

type Pop = Bool

type BramStack = BS
data BS = BS QIndex QIndex
	deriving (Eq, Generic, NFData, Show)

empty = BS 0 (-1)

topEntity = bramStack

bramStack :: Signal Push -> Signal Pop -> Signal (Maybe Pack)
bramStack push pop = fmap nextOut b <*> ram <*> push <*> pop
	where
		(read, write, b) = unbundle $ 
			(mealy bramStep BramStack.empty) $ bundle (push, pop)
		ram = setFirst (repeat 0) $ readNew (blockRam ramInitial) read write

		push' = register None push
		pop'  = register False pop

		ramInitial = repeat (repeat 0) :: Vec QSize Pack

bramStep b@(BS top btm) (push, pop) = (out, (top', write, b))
	where
		out = BS top' btm'

		top' = nextTop top btm push pop
		btm' = case push of
			Bottom p -> btm - 1
			_        -> btm
		
		write = insert b push

nextTop top btm push pop = top + pu
	where
		pu = case push of
			Top p    -> if
				| pop     -> 0
				| not pop -> 1
			Bottom p -> if
				| pop               -> (-1)
				| otherwise         -> 0
			None     -> if
				| pop && top /= btm -> (-1)
				| otherwise         -> 0

insert (BS top btm) push = case push of
			Top p    -> Just (top + 1, p)
			Bottom p -> Just (btm    , p)
			None     -> Nothing

nextOut b v push pop = case pop of
	False -> Nothing
	True  -> newTop b v push

newTop (BS top btm) v push = case push of
	Top p    -> Just p
	Bottom p -> if 
		| top == btm -> Just p
		| otherwise  -> Just v
	None     -> if
		| top == btm -> Nothing
		| otherwise  -> Just v
