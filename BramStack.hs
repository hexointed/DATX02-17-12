{-# LANGUAGE DeriveGeneric, DeriveAnyClass, MultiWayIf #-}

module BramStack  where

import Base
import Pack
import Float

type QSize = 16
type QIndex = Unsigned (CLog 2 QSize)

data Push
	= Top Pack
	| Bottom Pack
	| None
	deriving (Eq, Generic, NFData, Show)

type Pop = Bool

type BramStack = BS
data BS = BS QIndex QIndex (Vec QSize Pack)
	deriving (Eq, Generic, NFData, Show)

empty = BS 0 (-1) (repeat (repeat 0))

topEntity = bramStack 

bramStack :: Signal Push -> Signal Pop -> Signal (Maybe Pack)
bramStack = curry (mealy bramStep BramStack.empty . bundle)

bramStep b@(BS top btm v) (push, pop) = (out, next)
	where
		out = BS top' btm' v'
		next = case pop of
			False -> Nothing
			True  -> newTop b push
		
		top' = nextTop top btm push pop
		
		btm' = case push of
			Bottom p -> btm - 1
			_        -> btm
		
		v' = insert b push

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

insert (BS top btm v) push = replace ptr val v
	where
		(ptr, val) = case push of
			Top p    -> (top + 1, p)
			Bottom p -> (btm    , p)
			None     -> (top    , v !! top)

newTop (BS top btm v) push = case push of
	Top p    -> Just p
	Bottom p -> if 
		| top == btm -> Just p
		| otherwise  -> Just (v !! top)
	None     -> if
		| top == btm -> Nothing
		| otherwise  -> Just (v !! top)
