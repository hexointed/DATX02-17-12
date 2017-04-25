.data:
zero:
	0.0
one:
	1.0
two:
	2.0
displaysize:
	16.0
pos:
	7.5
radius:
	35.0
greenshift:
	256
red:
	65280.0
shift:
	256.0





epsilon:
	0.01
eyex: ;this vector might be removed once I get the hang of this
	0.0
eyey:
	0.0
eyez:
	0.0

lookatx:
	0.0
lookaty:
	0.0
lookatz:
	0.0

upVecx: ;this vector might be removed once I geet the hand of this
	0.0
upVecy:
	1.0
upVecz:
	0.0

ballr:
	10.0
ballx:
	0.0
bally:
	10.0
ballz:
	0.0




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

draw: 

		; ritar ut cirkel om placeras i draw
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


	pack 0
	a setval 15 0


		;calculates the length between temp vec and object
	val &ballx	; squares the difference in the x-axis
	pack 11
	sub
	a setval 14 0
	pack 14
	mul
	a setval 11 0

	val &bally
	pack 12
	sub
	a setval 14 0
	pack 14
	mul

	val &ballz 
	pack 13
	sub
	a setval 14 0
	pack 14
	mul

	pack 11
	pack 12
	pack 13
	add
	add
	sqrt


			; if done after "length", will deduce whether the march point is
			; sufficiently close to the surface
	val &epsilon
	;lessthan
	z 0 pushq 	; om epsilon är mindre än d, välj färg på pixel

	nz 0 pushq	; om epsilon är större än d, gör om hela skiten


;subrutiner

;powerfunktion, return x*x
pow:
	pack 14
	pack 14
	mul
	a setval 14 0
	pack 15
	a setval 0 0
	a pushq
	a drop
