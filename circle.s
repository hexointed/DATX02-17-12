.data:
one:
	1.0
zero:
	0.0
displaysize:
	16.0
pos:
	7.5
radius:
	35.0
greenshift:
	256

epsilon:
	0.01
ballr:
	10
ballx:
	0
bally:
	10
ballz:
	0


.text:
generate: ; skapar calcpos tråden för nuvarande pixel och generate tråden för nästa pixel
	next 1 ; comment
	val &calcpos
	pack 1
	val &one
	sub
	a setval 1 0
	nz 0 pushq ; pushar till kön
	a setval 0 1 
	a pushq
	a drop ; discardar tråden

calcpos: ; räknar ut vart på skärmen vi är och skapar en drawtråd
	next 2 
	pack 1
	val &displaysize
	div
	floor
	pack 1
	pack 1
	val &displaysize
	div
	floor
	val &displaysize
	mul
	sub
	val &draw
	a setval 0 0
	a setval 2 1
	a setval 3 2
	a pushq
	a drop

draw: ; hanterar det faktiska ritandet, här kan en raymarchingloop implementeras
	next 3
	pack 2
	val &pos
	sub
	pack 2
	val &pos
	sub
	mul
	pack 3
	val &pos
	sub
	pack 3
	val &pos
	sub
	mul
	add
	val &radius
	div
	floor
	val &greenshift
	mul
	a setval 2 0
	a pushf
	a drop




length:
				 	
			;calculates the current march pos and stores in temp vector
	pack 4
	pack 7
	mul
	pack 8
	add
	a setval 11 0

	pack 5
	pack 7
	mul
	pack 9
	add
	a setval 12 0

	pack 6
	pack 7
	mul
	pack 10
	add
	a setval 13 0



			;calculates the length between temp vec and object
	val &ballx
	pack 11
	sub
	pow

	val &bally
	pack 12
	sub
	pow

	val &ballz 
	pack 13
	sub
	pow

	pack 11
	pack 12
	pack 13
	add
	adiv
den övre delat på den undre

sub
den övre minus den undredd
	sqrt


			; if done after "length", will deduce whether the march point is
			; sufficiently close to the surface
	val &epsilon
	lessthan
	z pushq 	; om epsilon är mindre än d, välj färg på pixel

	nz pushq	; om epsilon är större än d, gör om hela skiten



