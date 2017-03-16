module Vector where

import Base
import Float

data Vector n a = Vector (Vec n a)

type Position = Vector 3 Float.Float

position x y z = Vector $ x :> y :> z :> Nil

x (Vector v) = at d0 v
y (Vector v) = at d1 v
z (Vector v) = at d2 v
w (Vector v) = at d3 v

zipVec f (Vector a) (Vector b) = Vector $ zipWith f a b

instance Functor (Vector n) where
	fmap f (Vector v) = Vector (fmap f v)

instance KnownNat n => Applicative (Vector n) where
	pure = Vector . pure
	Vector a <*> Vector b = Vector $ a <*> b

(.+) = zipVec (+)
(.-) = zipVec (-)
(.*) = foldr (+) . zipVec (*)

length = sqrt . foldr (+) . liftf (zipVec (*)) id id
