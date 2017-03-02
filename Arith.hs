{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Arith where

import Language.Haskell.TH
import Control.Monad
import CLaSH.Prelude
import Unsafe.Coerce
import Base
import Rec

rcAdder a b c = mapAccumR (\c (a, b) -> fullAdder a b c) c (zip a b)

claAdder a b c = undefined
	where
		single = undefined

claPropGen p g = foldr 
	(\(p1,g1) (p2,g2) -> (and' p1 p2, or' (and' g1 p2) g2)) (high, low) 
	(zip p g)

topEntity :: Vec 3 Bit -> Vec 3 Bit -> Bit -> (Vec 3 Bit, Bit, Bit)
topEntity = test

--class A a where
--	a :: a -> a
--
--instance {-# OVERLAPPING #-} A (Vec 1 a) where
--	a = id
--
--instance {-# OVERLAPPING #-} A (Vec 2 a) where
--	a = id
--
--instance {-# OVERLAPPING #-} A (Vec 3 a) where
--	a = undefined
--
--instance {-# OVERLAPPABLE #-} KnownNat n => A (Vec n a) where
--	a = undefined

aIf :: forall n . KnownNat n => Vec n Int -> Vec n Int -> Vec n Int
aIf = case toUNat (SNat @n) of
	USucc UZero         -> zipWith (+)
	USucc (USucc UZero) -> zipWith (+)
	_                   -> zipWith (+)
