default:
	clash Arith.hs --vhdl -clash-inline-limit=1000 -fno-warn-tabs

clean:
	@- rm *.hi *.o
