"
Day15 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay15',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay15 >> computeGps: grid forShape: shape [

	| res |
	res := 0.

	grid withIndexesDo: [ :c :r :v |
		v = shape ifTrue: [ res := res + (r  - 1 * 100 + c - 1) ] ].

	^ res
]

{ #category : 'as yet unclassified' }
AoCDay15 >> moveRobot: robot direction: dir onGrid: grid [

	| nextPos next result |
	nextPos := dir
		           caseOf: {
				           ([ '<' ] -> [ robot + (-1 @ 0) ]).
				           ([ '^' ] -> [ robot + (0 @ -1) ]).
				           ([ '>' ] -> [ robot + (1 @ 0) ]).
				           ([ 'v' ] -> [ robot + (0 @ 1) ]) }
		           otherwise: [ self error: 'Invalid direction' ].

	next := grid at: nextPos.

	"Transcript
		show: 'nextPos: ';
		show: nextPos asString;
		show: ', next: ';
		show: next asString;
		cr."

	next ifNotNil: [
			result := next
				          caseOf: {
						          ([ '.' ]
						           -> [ self swapItems: robot with: nextPos in: grid ]).
						          ([ 'O' ] -> [
							           | retPos |
							           retPos := self
								                     moveRobot: nextPos
								                     direction: dir
								                     onGrid: grid.
							           retPos = nextPos
								           ifTrue: [ robot ]
								           ifFalse: [
								           self swapItems: robot with: nextPos in: grid ] ]).
						          ([ '#' ] -> [ robot ]) }
				          otherwise: [ robot ] ].

	"Transcript
		show: '----------';
		cr.
	grid printGridOn: Transcript."

	^ result
]

{ #category : 'parsing' }
AoCDay15 >> parse: lines [

	| sections grid moves |
	sections := lines splitOn: [ :e | e isEmpty ].

	grid := self parseToGridAsStrings: sections first.

	moves := (sections second joinUsing: '') asArray collect: [ :c | c asString ].

	^ {
		  grid.
		  moves }
]

{ #category : 'executing' }
AoCDay15 >> part1: lines [

	| output grid moves width height robot |
	output := self parse: lines.
	grid := output first.
	moves := output second.
	width := grid width - 2.
	height := grid height - 2.

	grid withIndexesDo: [ :row :col :item |
		item = '@' ifTrue: [ robot := col @ row ] ].

	"grid printGridOn: Transcript."

	moves do: [ :dir |
		robot := self moveRobot: robot direction: dir onGrid: grid ].

	Transcript
		show: '-----';
		cr.

	"grid printGridOn: Transcript."

	^ self computeGps: grid forShape: 'O'
]

{ #category : 'as yet unclassified' }
AoCDay15 >> swapItems: from with: to in: grid [

	| fromValue toValue |
	toValue := grid at: to.
	fromValue := grid at: from.

	grid at: to put: fromValue.
	grid at: from put: toValue.

	^ to
]
