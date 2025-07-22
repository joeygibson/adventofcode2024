Class {
	#name : 'AoCDay04',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'enumerating' }
AoCDay04 >> findAllStarts: puzzle [
	| starts |
	
	starts := OrderedCollection new.
	
	puzzle rowAndColumnValuesDo: [ :row :col :value |
		value = $X ifTrue: [ 
			starts add: col@row ] ].
	
	^ starts.
]

{ #category : 'as yet unclassified' }
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
	
	starts := self findAllStarts: puzzle.
	
	res := starts flatCollect: [ :start |
		|neighbors|
		neighbors := self getNeighbors: start. 
		
		neighbors select: [ :neighbor |
			self spellsXmas: puzzle neighbors: neighbor]].
	
	^ res size.
]

{ #category : 'as yet unclassified' }
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
