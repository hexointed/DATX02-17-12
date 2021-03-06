{-# LANGUAGE MultiWayIf #-}

module Inst 
		(Bits', Line, Word, cleanupCode, assembleInst) 
	where

import CLaSH.Prelude hiding (tail, Word, lines, zip, map, (++))
import Data.List
import Prelude hiding (Word)

type Bits' n = Either String (BitVector n)
type Line = String
type Word = String

cleanupCode :: [Line] -> [(Int, Line)]
cleanupCode = 
	filter (not . isDirective . snd) .
	filter (not . isEmpty . snd) .
	filter (not . isNumber . snd) .
	map filterLabels .
	zip [1 ..] 
	where
		isEmpty line = words line == []
		isNumber line = case reads line :: [(Double,String)] of
			[] -> False
			_  -> error "There is a line in a \".text:\" block that is only a number."

		isDirective ('.':line) = True
		isDirective _          = False

		filterLabels (nr, line)
			| ":" `isInfixOf` line = (,) nr $ tail $ dropWhile (/=':') line
			| otherwise            = (nr, line)

assembleInst :: Line -> Bits' 16
assembleInst str = case words str of
	[]   -> Left "Expected instruction, found nothing"
	w:ws -> if
		| isCond w    -> assembleCFU (w:ws)
		| w == "next" -> assembleNext ws
		| w == "val"  -> assembleVal ws
		| w == "pack" -> assemblePack ws
		| otherwise   -> assembleDFU (w:ws)

assembleCFU :: [Word] -> Bits' 16
assembleCFU ws = case ws of
	c:inst -> ass c inst
	where
		ass a c = do
			con <- cond a
			tra <- tran c
			return $ 0 ++# con ++# tra

		tran :: [Word] -> Bits' 12
		tran ws = do
			name <- head' ws
			case name of
				"pushf"  -> return (0 ++#) <*> args 0 []
				"pushs"  -> return (1 ++#) <*> args 0 []
				"pushq"  -> return (2 ++#) <*> args 0 []
				"drop"   -> return (3 ++#) <*> args 0 []
				"setval" -> return (4 ++#) <*> args 2 (tail ws)
				_        -> Left "Unrecognized instruction"

		args :: Int -> [Word] -> Bits' 8
		args 0 []       = return 0
		args 2 (a:b:[]) = do
			n1 <- readUnsigned d4 a
			n2 <- readUnsigned d4 b
			return $ n1 ++# n2
		args _ _ = Left "Wrong number of arguments"
		
		cond :: Word -> Bits' 3
		cond "a"  = Right 0
		cond "z"  = Right 1
		cond "nz" = Right 2
		cond "n"  = Right 3

assembleVal ws = do
	n <- single ws
	n <- readUnsigned d12 n
	return $ (0xD :: BitVector 4) ++# n 

assemblePack ws = do
	n <- single ws
	n <- readUnsigned d4 n
	return $ (0xC :: BitVector 4) ++# 0 ++# n

assembleNext ws = do
	n <- single ws
	n <- readUnsigned d12 n
	return $ (0xE :: BitVector 4) ++# n

assembleDFU ws = do
	n <- single ws
	i <- case n of
		"max"   -> Right 0x00
		"min"   -> Right 0x01
		"add"   -> Right 0x02
		"sub"   -> Right 0x03
		"mul"   -> Right 0x04
		"div"   -> Right 0x05
		"sqrt"  -> Right 0x06
		"abs"   -> Right 0x07
		"floor" -> Right 0x08
		"accum" -> Right 0x09
		"dot"   -> Right 0x0a
		"cross" -> Right 0x0b
		"addv"  -> Right 0x0c
		"subv"  -> Right 0x0d
		"scale" -> Right 0x0e
		"copy"  -> Right 0x0f
		_       -> Left $ "Unrecognized instruction: " ++ unwords ws
	return $ (2 :: BitVector 2) ++# (i :: BitVector 6) ++# 0

readUnsigned s n = fmap pack $ read' s n 
	where
		read' :: KnownNat n => SNat n -> Word -> Either String (Unsigned n)
		read' _ n = case reads n of
			[(n', "")] -> Right n'
			[]         -> Left "Expected number"

isCond w = case filter (==w) ["a", "nz", "z", "n"] of
	[] -> False
	_  -> True

single (l:[]) = Right l
single _      = Left "Wrong number of arguments"

head' (l:ls) = Right l
head' _      = Left "Not enough arguments"
