.data:
zero:
	0
one:
	1.0
two:
	2.0
three:
	3.0
four:
	4.0
five:
	5.0
six:
	6.0
displaysize:
	16.0
pos:
	7.5
radius:
	4.0
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

	val &zero ; z
	pack 2    ; y 
	val &pos  ; y
	sub
	pack 3    ; x
	val &pos  ; x
	sub

	val &zero ; z
	pack 2    ; y 
	val &pos  ; y
	sub
	pack 3    ; x
	val &pos  ; x
	sub

	dot
	sqrt

	val &one
	val &one
	val &one

	val &two
	val &one
	val &five
	
	val &four
	val &three
	val &five

	cross
	dot

	div
	floor
	val &greenshift
	mul
	a setval 2 0
	a pushf
	a drop
