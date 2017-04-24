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
topQueue inputs = undefined
	where
		out = makeDecision (fmap calcDecision inputs)
		wantsDelayed = register (repeat False)   $ fmap (map fst) inputs
		has'sDelayed = register (repeat Nothing) $ fmap (map snd) inputs

noMultiAcc :: Vec n (Maybe Pack) -> Vec n Bool -> Vec n (Maybe Pack)
noMultiAcc v v' = zipWith f v v'
	where
		f p a
			| a         = Nothing
			| otherwise = p

makeDecision :: Signal Decision -> Signal (Maybe Pack)
makeDecision d = mux select bram altPack
	where
		bram = fmap snd $ bramStack (fmap decision d) (fmap packFromDecision d)
		select = register True $ fmap decisionSelector d
		altPack = register (Just $ repeat 0) (fmap packFromDecision' d)

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

decisionSelector :: Decision -> Bool
decisionSelector (Send p) = False
decisionSelector Nop      = False
decisionSelector _        = True

decision :: Decision -> (Bool, Bool)
decision (Nop   ) = dup False
decision (Pop   ) = (False, True)
decision (Push p) = (True, False)
decision (Send p) = dup False

packFromDecision :: Decision -> Pack
packFromDecision (Send p) = p
packFromDecision (Push p) = p
packFromDecision _        = repeat 0

packFromDecision' :: Decision -> Maybe Pack
packFromDecision' (Send p) = Just p
packFromDecision' (Push p) = Just p
packFromDecision' Nop      = Nothing
packFromDecision' Pop      = Nothing

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
