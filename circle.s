.data:
zero:
	0.0
one:
	1.0
minusone:
	-1.0
two:
	2.0
hundred:
	100.0
ones:
	255.0
displaysizex:
	32.0
displaysizey:
	16.0
numPixels:
	512.0
red:
	65280.0
shift:
	256.0
white:
	65535.0

maxDist:
	20.0
epsilon:
	0.01

eyex:
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
	1.0

rightx:
	1.0
righty:
	0.0
rightz:
	0.0

upx:
	0.0
upy:
	1.0
upz:
	0.0

ballr:
	6.0
ballx:
	0.0
bally:
	0.0
ballz:
	10.0

greenshift:
	256

.text:

initialize:
	val &generate
	a setval 0 0
	val &numPixels
	a setval 1 0
	a pushq
	a drop

generate: ; skapar calcpos tråden för nuvarande pixel och generate tråden för nästa pixel
	next 1 ; comment
	pack 1
	val &one
	sub
	a setval 1 0
	nz 0 pushq ; pushar till kön
	val &calcpos
	a setval 0 0
	a pushq
	a drop ; discardar tråden

calcpos: ; räknar ut vart på skärmen vi är och skapar en drawtråd
	next 2 
	pack 1
	val &displaysizex
	div
	floor
	pack 1
	pack 1
	val &displaysizex
	div
	floor
	val &displaysizex
	mul
	sub
	val &draw
	a setval 0 0
	a setval 2 1
	a setval 3 2
	a pushq
	a drop


draw: 

;programflow: camsetup -> raypos -> distball -> hit? -> went too far? -> march one step

camSetup: 
	; sätter upp ray-direction registren
	; args:   reg. 1-2
	; result: reg. 4-6
	; alters: reg. 8-10

	; calculate a directional vector which will be in the center of the view, 
	; stores in ray-direction. This will be used to offset with later.
	val &lookatx
	val &lookaty
	val &lookatz
	val &eyex
	val &eyey
	val &eyez
	subv
	a setval 4 2
	a setval 5 1
	a setval 6 0

	; calculate screen positions as a range from 1 to -1, 
	; store in reg. 14 and 15
	pack 2
	val &displaysizex
	val &one
	sub
	val &two
	div
	div
	val &one 
	sub
	a setval 14 0


	pack 3
	val &displaysizey
	val &one
	sub
	val &two
	div
	div
	val &one
	sub
	val &minusone	; (1,1) should be in the upper right corner, 
					; not the lower right corner, so the y-value is negated.
	mul
	a setval 15 0



	; calculate and scale upv and rightv
	val &rightx
	val &righty
	val &rightz
	pack 14
	scale

	val &upx
	val &upy
	val &upz
	pack 15
	scale

	; add the x-vector, the y-vector and the directional vector.
	addv

	pack 4
	pack 5
	pack 6

	addv

	a setval 8 2
	a setval 9 1
	a setval 10 0


normalize:
	; creates vector that has the same length as the previous but with
	; a length of 1
	; args:   reg. 8-10
	; result: reg. 8-10

	pack 8
	pack 9
	pack 10

	; calculates length of vector
	pack 8
	pack 8
	mul
	pack 9
	pack 9
	mul
	pack 10
	pack 10 
	mul
	add
	add
	sqrt

	scale

	a setval 8 2
	a setval 9 1
	a setval 10 0

camCont:
	; continue setting up the camera

	pack 8
	a setval 4 0
	pack 9
	a setval 5 0
	pack 10
	a setval 6 0
	val &one
	a setval 7 0

	val &afterSetup
	a setval 0 0
	a pushq
	a drop

afterSetup:
	; clear all registers

	val &zero
	a setval 2 0
	a setval 3 0
	a setval 7 0
	a setval 8 0
	a setval 9 0
	a setval 10 0
	a setval 11 0
	a setval 12 0
	a setval 13 0
	a setval 14 0
	a setval 15 0


rayPos:
		; calculates the current march pos and stores in tempVec
		; args:    reg. 4-7
		; results: reg. 8-10

	pack 4
	pack 5
	pack 6
	pack 7

	scale

	a setval 8 2
	a setval 9 1
	a setval 10 0

distBall:
	; calculates the length between tempVec and Ball, ball should
	; probably be easy to substitute ball with another object
	; args: reg. 8-10 
	; result: reg. 14
	; note: alters reg. 11-13

	val &ballx	
	pack 8
	sub
	copy
	mul

	val &bally
	pack 9
	sub
	copy
	mul

	val &ballz 
	pack 10
	sub
	copy
	mul

	add
	add
	sqrt

	val &ballr
	sub
	a setval 14 0

hit:
	val &zero 
	pack 14
	val &epsilon
	sub

	; if result is negative algorithm is finished
	n 0 setval 2 1
	n 0 pushf
	n 0 drop

	;otherwie increase scaler
	pack 14
	pack 7
	add
	a setval 7 0

tooFar:
	; We'we marched too many steps and should stop

	val &white
	val &hundred
	pack 7
	sub
	n 0 setval 2 1
	n 0 pushf
	n 0 drop

nextStep:
	; We should continue marching more steps

	val &rayPos
	a setval 0 0
	a pushq
	a drop
