module CFU where

data Choice
	= NZ
	| Z

data Cond
	= Cond Choice StackPtr

type PackPtr = Unsigned 4
type StackPtr = Unsigned 4

data Action
	= PushQ
	| PushF
	| Drop
	| SetVal PackPtr StackPtr

data Instr
	= Instr Cond Action

--------------------------------------------------------------------------------

type IPtr = Unsigned 8
type Pack = Vec 8 Float

data CFU = CFU IPtr Pack

step :: CFU -> (Stack Float, Maybe Instr) -> (CFU, Maybe Pack)
step (CFU iptr pack) (st, inst) = case inst of
	Nothing          -> (CFU iptr pack, Nothing)
	Just (Instr c a) -> (CFU (iptr + 1)

upack a p s = case a of
	PushQ            -> Just p
	PushF            -> Just p
	Drop             -> Just p
	SetVal pptr sptr -> replace pptr (topN sptr s) p
