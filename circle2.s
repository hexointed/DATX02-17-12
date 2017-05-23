.data:
one:
	1.0
greenshift:
	256
numPixels:
	512.0
.text:
initialize:
	val &generate
	a setval 0 0
	val &numPixels
	a setval 1 0
	a pushq
	a drop

generate:
	next 1
	pack 1
	val &one
	sub
	a setval 1 0
	nz pushq
	val &calcpos
	a setval 0 0
	a pushq
	a drop

calcpos:
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
	val &scalex
	div
	val &draw
	a setval 0 0
	a setval 2 1
	a setval 3 2
	a pushq
	a drop

draw:
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

