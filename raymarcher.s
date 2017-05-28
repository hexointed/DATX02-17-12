.data:
zero:
	0.0
one:
	1.0
minusone:
	-1.0
two:
	2.0
sqrt2:
	1.41421356
	
displaysizex:
	64.0
displaysizey:
	64.0
white:
	65535.0
maxDist:
	20.0
epsilon:
	0.01

.text:

initialize:
	; set the number of pixels / top pixel pointer
	; args: none
	; returns: reg 1 - highest pixel pointer

	val &displaysizex
	val &displaysizey
	mul
	a setval 1 0

generate: 
	; generate a thread for each pixel
	; args: reg 1 - no. of pixels to create
	; returns: reg 1 - pixel pointer

	val &generate
	a setval 0 0
	pack 1
	val &one
	sub
	a setval 1 0
	nz pushq

calcpos: 
	; calculate the x, y coordinate of the current pixel
	; args: reg 1 - pixel pointer
	; returns:
	; 	reg 1 - pixel pointer
	; 	reg 2 - x coordinate
	; 	reg 3 - y coordinate
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
	a setval 2 0
	a setval 3 1

;programflow: camsetup -> raypos -> distball -> hit? -> went too far? -> march one step

camSetup: 
	; sätter upp ray-direction registren
	; args:   reg 2,3   - x,y coordinates
	; result: 
	; 	reg 2,3   - x,y coordinates, normalized to range [-1, 1]
	; 	reg 4,5,6 - march direction
	; 	reg 7     - march distance (zero)

.data:

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

.text:
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
	; store in reg. 2 and 3
	pack 2
	val &displaysizex
	val &one
	sub
	val &two
	div
	div
	val &one 
	sub
	a setval 2 0

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
	a setval 3 0

	; calculate and scale upv and rightv
	val &rightx
	val &righty
	val &rightz
	pack 2
	scale

	val &upx
	val &upy
	val &upz
	pack 3
	scale

	; add the x-vector, the y-vector and the directional vector.
	addv

	pack 4
	pack 5
	pack 6

	addv

	a setval 4 2
	a setval 5 1
	a setval 6 0


normalize:
	; Creates a vector with same direction as input, but with length 1.
	; args:   reg 4,5,6
	; result: reg 4,5,6

	pack 4
	pack 5
	pack 6

	; calculates length of vector
	pack 4
	pack 4
	mul
	pack 5
	pack 5
	mul
	pack 6
	pack 6 
	mul
	add
	add
	sqrt

	scale

	a setval 4 2
	a setval 5 1
	a setval 6 0

camCont:
	; continue setting up the camera

	val &zero
	a setval 7 0

rayPos:
		; calculates the current march pos and stores in tempVec
		; args:    
		; 	reg 4,5,6  - march direction
		; 	reg 7      - total marched distance
		; results: 
		; 	reg 8,9,10 - current location
		; 	reg 11     - currint epsilon

	pack 4
	pack 5
	pack 6
	pack 7

	scale

	a setval 8 2
	a setval 9 1
	a setval 10 0
	
	pack 7
	val &two
	mul
	val &epsilon
	mul
	a setval 11 0

distBall:
	; calculates the length between tempVec and Ball, ball should
	; probably be easy to substitute ball with another object
	; args: reg. 8-10 
	; result: reg. 14

.data:

	ballr:
		6.0
	ballx:
		0.0
	bally:
		0.0
	ballz:
		10.0

.text:
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
	pack 11
	sub

	; if result is negative algorithm is finished
	n setval 2 1
	n pushf
	n drop

	;otherwie increase scaler
	pack 14
	pack 7
	add
	a setval 7 0

tooFar:
	; We'we marched too far and should stop

	val &white
	val &maxDist
	pack 7
	sub
	n setval 2 1
	n pushf
	n drop

nextStep:
	; We should continue marching more steps

	val &rayPos
	a setval 0 0
	a pushq
	a drop
