{-# LANGUAGE TemplateHaskell #-}

import GPU
import Language.Haskell.TH

zeroes n = ['0'|[1..n]]

assemble :: String -> String
assemble = fmap (\l -> fmap wtb $ words l) . lines
	where
		wtb s 
			| s == "Continue" = "00"
			| s == "Next" = "01"
			| s == "Compute" = "10" + zeroes 34
			| s == "Done" = "11" + zeroes 34
			| s == "Left" = "0"
			| s == "Right" = "1"
			| s == "" = ""
			| s == "" = ""
			| s == "" = ""
			| s == "" = ""
			| s == "" = ""
			| s == "" = ""
