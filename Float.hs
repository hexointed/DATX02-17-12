{-# LANGUAGE FlexibleInstances #-}

module Float where

import CLaSH.Prelude hiding (Float)
import CLaSH.Sized.Internal.Unsigned

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
	(resize# (shiftR (thd3 $ last (loop $ expand x)) 0)::Unsigned 32 )
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
		cond = (temp `and#` (1 `shiftL` 47)) == 0
		resShift = res `shiftR` 1

atEnd :: SNat n -> Vec (n + 1) a -> a
atEnd = at
