module Container where

import Base

class Alternative c => Container c where
	filled :: a -> c a
	push :: a -> c a -> c a
	pop :: c a -> c a
	top :: c a -> a
