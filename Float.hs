{-# LANGUAGE FlexibleInstances, RankNTypes #-}

module Float where

import CLaSH.Sized.Internal.Unsigned
import Base

type GenFloat (a :: Nat) = Fixed Signed a a
type Float = GenFloat 16
type Point = Unsigned 4

instance Floating (GenFloat 16) where
	sqrt = sqrtFS
	pi = undefined
	exp = undefined
	log = undefined
	sin = undefined
	cos = undefined
	asin = undefined
	acos = undefined
	atan = undefined
	sinh = undefined
	cosh = undefined
	asinh = undefined
	acosh = undefined
	atanh = undefined

topEntity = mealy (\st inp -> (\x -> (x,x)) $ sqrtFS inp + st) 0

-- square root on 16 16 signed fixed numbers
-- 24 iterations on 48 internal bits
sqrtFS :: Float -> Float
sqrtFS x = 
	bitCoerce 
	(resize# (shiftR (thd3 (loopN $ expand x)) 0)::Unsigned 32 )
	where
		expand :: Fixed Signed 16 16 -> Unsigned 48
		expand x = bitCoerce (resizeF x :: SFixed 16 32) :: Unsigned 48

		thd3 (_,_,x) = x

loop :: Unsigned 48 -> Vec 24 (Unsigned 48, Unsigned 48, Unsigned 48)
loop x = generate d24 
	loopStep
	(startbit, x ,0) -- these are the starting values for bit, num, res
	where 
		startbit :: Unsigned 48
		startbit = shiftL 1 46

loopStep (bit, num, res) =
	( bit `shiftR` 2
	, if cond then temp else num
	, if cond then resShift `or#` bit else resShift
	)
	where
		temp = num - (res `or#` bit)		
		cond = isNegative temp
		resShift = res `shiftR` 1

isNegative :: KnownNat n => Unsigned (n + 1) -> Bool
isNegative x = (x `and#` (1 `shiftL` fromInteger (natVal $ y x))) == 0
	where
		y = undefined :: KnownNat n => Unsigned (n + 1) -> Unsigned n
	
type Step n = (Unsigned n, Unsigned n, Unsigned n)

loopN :: Unsigned 48 -> Step 48
loopN x =
	loopStepN d24 .
	loopStepN d23 .
	loopStepN d22 .
	loopStepN d21 .
	loopStepN d20 .
	loopStepN d19 .
	loopStepN d18 .
	loopStepN d17 .
	loopStepN d16 .
	loopStepN d15 .
	loopStepN d14 .
	loopStepN d13 .
	loopStepN d12 .
	loopStepN d11 .
	loopStepN d10 .
	loopStepN d9 .
	loopStepN d8 .
	loopStepN d7 .
	loopStepN d6 .
	loopStepN d5 .
	loopStepN d4 .
	loopStepN d3 .
	loopStepN d2 .
	loopStepN d1 $
	(startbit, x ,0) -- these are the starting values for bit, num, res
	where 
		startbit :: Unsigned 48
		startbit = shiftL 1 46

loopStepN n (bit, num, res) =
	( bit `shiftR` 2
	, if cond then temp else num
	, if cond then resShift `or#` bit else resShift
	)
	where
		temp = subHighBits (mulSNat n d2) num (res `or#` bit)
		cond = isNegative temp
		resShift = res `shiftR` 1

subHighBits :: (KnownNat m, KnownNat n) =>
	SNat n -> Unsigned (n + m) -> Unsigned (n + m) -> Unsigned (n + m)
subHighBits n a b = unpack $
	(++#)
	(pack# (highBits n a - highBits n b))
	(pack# $ lowBits n a)

highBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned n
highBits n = unpack . bTake n . pack#

lowBits :: (KnownNat n, KnownNat m) => SNat n -> Unsigned (n + m) -> Unsigned m
lowBits n u = resize u
