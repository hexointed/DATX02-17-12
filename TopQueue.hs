{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module TopQueue where

import Base
import Data.Maybe
import Core hiding (pack)
import Pack
import CLaSH.Sized.Fixed
import CLaSH.Class.BitPack
import CLaSH.Sized.BitVector
import BramStack

type Address = Signed 32
type RGBValue = BitVector 24

type VecIn n = Vec n (Bool, Maybe Pack)
type VecOut n = Vec n (Maybe Pack, Bool)

data Decision
	= Nop
	| Pop
	| Push Pack
	| Send Pack
	deriving (Eq, Generic, Show, NFData)

topQueue :: KnownNat n => Signal (VecIn (n + 1)) -> Signal (VecOut (n + 1))
topQueue inputs = fmap coresInterface inputsDelayed <*> packOut
	where
		out = fmap calcDecision inputs
		inputsDelayed = register (repeat (False, Nothing)) inputs
		packDelayed = register None $ fmap packFromDecision out
		bram = bramStack (fmap packFromDecision out) (fmap needsPop out)
		packOut = bram

coresInterface :: VecIn n -> Maybe Pack -> VecOut n
coresInterface v p = zipWith (,) outs acc
	where
		acc = accCorrect (map snd v)
		outs = giveBackCorrect (map fst v) p

accCorrect :: Vec n (Maybe Pack) -> Vec n Bool
accCorrect v = snd $ mapAccumL onlyFirst True v
	where
		onlyFirst isFirst pack = case isFirst && pack /= Nothing of
			True ->  (False, True)
			False -> (isFirst, False)

giveBackCorrect :: Vec n Bool -> Maybe Pack -> Vec n (Maybe Pack)
giveBackCorrect v p = snd $ mapAccumL onlyFirst True v
	where
		onlyFirst isFirst wants = case isFirst && wants of
			True  -> (False, p)
			False -> (isFirst, Nothing)

needsPop :: Decision -> Bool
needsPop (Pop) = True
needsPop _     = False

packFromDecision :: Decision -> Push
packFromDecision (Send p) = None
packFromDecision (Push p) = Bottom p
packFromDecision Nop      = None
packFromDecision Pop      = None

calcDecision :: VecIn (n + 1) -> Decision
calcDecision inputs = case packIn of
	Nothing -> case packOut of
		False -> Nop
		True  -> Pop
	Just p  -> case packOut of
		False -> Push p
		True  -> Send p
	where
		packIn = snd $ fold hasPack inputs
		packOut = fst $ fold wantsPack inputs

hasPack (w, p) (w', p') = case p of
	Just _  -> (w, p)
	Nothing -> (w', p')

wantsPack (w, p) (w', p')
	| w         = (w, p)
	| otherwise = (w', p')
