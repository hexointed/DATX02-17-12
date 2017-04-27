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

type VecIn n = Vec n (Bool, Maybe Pack)
type VecOut n = Vec n (Maybe Pack, Bool)

topQueue :: KnownNat n => Signal (VecIn (n + 1)) -> Signal (VecOut (n + 1))
topQueue inputs = fmap coresInterface inputs <*> bram
	where
		(has, wants) = unbundle $ fmap calcDecision inputs
		bram = bramStack (fmap packFromMaybe has) wants

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

packFromMaybe :: Maybe Pack -> Push
packFromMaybe Nothing  = None
packFromMaybe (Just p) = Bottom p

calcDecision :: VecIn (n + 1) -> (Maybe Pack, Bool)
calcDecision inputs = (has, wants)
	where
		has = snd $ fold hasPack inputs
		wants = fst $ fold wantsPack inputs

hasPack (w, p) (w', p') = case p of
	Just _  -> (w, p)
	Nothing -> (w', p')

wantsPack (w, p) (w', p')
	| w         = (w, p)
	| otherwise = (w', p')
