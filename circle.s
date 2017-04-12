.data:
one:
	1.0
displaysize:
	16.0
pos:
	7.5
radius:
	35.0
greenshift:
	256

.text:
generate:
	next 1 ; comment
	val &calcpos
	pack 1
	val &one
	sub
	a setval 1 0
	nz 0 pushq
	a setval 0 1
	a pushq
	a drop

calcpos:
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
