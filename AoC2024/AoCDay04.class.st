Class {
	#name : 'AoCDay04',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'enumerating' }
AoCDay04 >> findAllInstances: puzzle of: letter [
	| starts |
	
	starts := OrderedCollection new.
	
	puzzle rowAndColumnValuesDo: [ :row :col :value |
		value = letter ifTrue: [ 
			starts add: col@row ] ].
	
	^ starts.
]

{ #category : 'executing' }
AoCDay04 >> getCorners: spot [

	| x y topLeft topRight bottomLeft bottomRight |
	x := spot x.
	y := spot y.

	topLeft := x - 1 @ (y - 1).
	topRight := x + 1 @ (y - 1).
	bottomLeft := x - 1 @ (y + 1).
	bottomRight := x + 1 @ (y + 1).

	^ {
		  topLeft.
		  topRight.
		  bottomLeft.
		  bottomRight }
]

{ #category : 'executing' }
AoCDay04 >> getNeighbors: spot [

	| x y right left up down upLeft upRight downLeft downRight |
	x := spot x.
	y := spot y.
	right := OrderedCollection new.
	left := OrderedCollection new.
	up := OrderedCollection new.
	down := OrderedCollection new.
	upLeft := OrderedCollection new.
	upRight := OrderedCollection new.
	downLeft := OrderedCollection new.
	downRight := OrderedCollection new.

	0 to: 3 do: [ :i |
			right add: x + i @ y.
			left add: x - i @ y.
			up add: x @ (y - i).
			down add: x @ (y + i).
			upLeft add: x - i @ (y - i).
			upRight add: x + i @ (y - i).
			downLeft add: x - i @ (y + i).
			downRight add: x + i @ (y + i) ].

	^ {
		  right.
		  left.
		  up.
		  down.
		  upLeft.
		  upRight.
		  downLeft.
		  downRight }
]

{ #category : 'executing' }
AoCDay04 >> part1: puzzle [
	| starts res |
	
	starts := self findAllInstances: puzzle of: $X.
	
	res := starts flatCollect: [ :start |
		|neighbors|
		neighbors := self getNeighbors: start. 
		
		neighbors select: [ :neighbor |
			self spellsXmas: puzzle neighbors: neighbor]].
	
	^ res size.
]

{ #category : 'executing' }
AoCDay04 >> part2: puzzle [

	| starts res |
	starts := self findAllInstances: puzzle of: $A.

	res := starts select: [ :spot | self xMas: puzzle atSpot: spot ].

	^ res size
]

{ #category : 'executing' }
AoCDay04 >> spellsXmas: puzzle neighbors: coords [

	| matches allValid |
	matches := 0.

	allValid := coords allSatisfy: [ :coord |
			            coord > (0 @ 0) and:
				            (coord x <= puzzle width and: coord y <= puzzle height) ].

	allValid ifFalse: [ ^ false ].

	'XMAS' with: coords do: [ :letter :coord |
			| value |
			value := puzzle atColumn: coord x atRow: coord y.
			value = letter ifTrue: [ matches := matches + 1 ] ].

	^ matches = 4
]

{ #category : 'executing' }
AoCDay04 >> xMas: puzzle atSpot: spot [

	| matches allValid topLeft topRight bottomLeft bottomRight combos corners found |
	matches := 0.

	corners := self getCorners: spot.

	allValid := corners allSatisfy: [ :coord |
			            coord > (0 @ 0) and:
				            (coord x <= puzzle width and: coord y <= puzzle height) ].

	allValid ifFalse: [ ^ false ].
	combos := #( 'MSMS' 'SMSM' 'MSSM' 'SMMS' ).

	topLeft := puzzle atColumn: corners first x atRow: corners first y.
	topRight := puzzle atColumn: corners second x atRow: corners second y.
	bottomLeft := puzzle atColumn: corners third x atRow: corners third y.
	bottomRight := puzzle
		               atColumn: corners fourth x
		               atRow: corners fourth y.

	found := combos
		         detect: [ :combo |
				         topLeft = combo first and:
					         (bottomRight = combo second and:
						          (topRight = combo third and: bottomLeft = combo fourth)) ]
		         ifNone: nil.

	^ found isNotNil
]
