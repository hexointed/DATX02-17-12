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
import DFU

type VecIn n = Vec n (Bool, Maybe (Pack, PackType))
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

accCorrect :: Vec n (Maybe a) -> Vec n Bool
accCorrect v = snd $ mapAccumL onlyFirst True v
	where
		onlyFirst isFirst Nothing = (isFirst, False)
		onlyFirst isFirst pack    = (False, isFirst)

giveBackCorrect :: Vec n Bool -> Maybe a -> Vec n (Maybe a)
giveBackCorrect v p = snd $ mapAccumL onlyFirst True v
	where
		onlyFirst isFirst wants = case isFirst && wants of
			True  -> (False, p)
			False -> (isFirst, Nothing)

packFromMaybe :: Maybe (Pack, PackType) -> Push
packFromMaybe (Just (p, Queue)) = Bottom p
packFromMaybe (Just (p, Stack)) = Top p
packFromMaybe _                 = BramStack.None

calcDecision :: VecIn (n + 1) -> (Maybe (Pack, PackType), Bool)
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
