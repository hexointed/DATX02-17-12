simulate: *.hs *.s
	clash -fno-warn-tabs Main.hs -O3 -o a.out

clean:
	@- rm *.hi *.o
