"
Day06 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay06',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay06 >> createMapAndGuard: lines [

	| map guard |
	map := CTArray2D width: lines first size height: lines size.

	lines doWithIndex: [ :line :row |
			line asArray doWithIndex: [ :c :col |
					| thing |
					thing := self createThing: c.
					(thing isKindOf: Guard) ifTrue: [
							thing col: col.
							thing row: row.
							guard := thing.
							thing := self createThing: $. ].
					map atColumn: col atRow: row put: thing ] ].

	^ {
		  map.
		  guard }
]

{ #category : 'as yet unclassified' }
AoCDay06 >> createThing: chr [

	chr = $^ ifTrue: [ ^ Guard new ].

	chr = $. ifTrue: [ ^ FloorTile new ].

	chr = $# ifTrue: [ ^ Furniture new ].

	chr = $O ifTrue: [ ^ Obstacle new ]
]

{ #category : 'as yet unclassified' }
AoCDay06 >> part1: lines [

	| map guard parts tiles |
	parts := self createMapAndGuard: lines.
	map := parts first.
	guard := parts second.

	self play: map withGuard: guard.

	tiles := map contents select: [ :thing | thing isKindOf: FloorTile ].

	^ tiles count: [ :thing | thing visited ]
]

{ #category : 'as yet unclassified' }
AoCDay06 >> part2: lines [

	| map guard parts guardCol guardRow loopCount |
	parts := self createMapAndGuard: lines.
	map := parts first.
	guard := parts second.
	guardCol := guard col.
	guardRow := guard row.
	loopCount := 1.

	1 to: map height do: [ :row |
			1 to: map width do: [ :col |
					| mapCopy guardCopy results |
					mapCopy := map deepCopy.
					guardCopy := guard deepCopy.

					(((map atColumn: col atRow: row) isKindOf: FloorTile) and:
						 (col ~= guardCol and: row ~= guardRow)) ifTrue: [
							mapCopy atColumn: col atRow: row put: (self createThing: $O).
							results := self play: mapCopy withGuard: guardCopy.
							results = #InALoop ifTrue: [ loopCount := loopCount + 1 ] ] ] ].

	^ loopCount
]

{ #category : 'as yet unclassified' }
AoCDay06 >> play: map withGuard: guard [

	| guardDirection oldX oldY x y nextSpot newDirection |
	[
		guardDirection := guard direction.
		oldX := guard col.
		oldY := guard row.
		x := oldX.
		y := oldY.

		guardDirection = #left
			ifTrue: [ x := oldX - 1 ]
			ifFalse: [ guardDirection = #right ifTrue: [ x := oldX + 1 ] ].

		guardDirection = #up
			ifTrue: [ y := oldY - 1 ]
			ifFalse: [ guardDirection = #down ifTrue: [ y := oldY + 1 ] ].

		(x > 0 and: (x <= map width and: (y > 0 and: y <= map height)))
			ifFalse: [ ^ #OutOfBounds ].

		nextSpot := map atColumn: x atRow: y.

		(nextSpot isKindOf: FloorTile)
			ifTrue: [
					nextSpot visitCount > 5 ifTrue: [ ^ #InALoop ].
					nextSpot visited: true.
					nextSpot addVisit.
					guard col: x.
					guard row: y ]
			ifFalse: [
					guardDirection = #up ifTrue: [ newDirection := #right ].
					guardDirection = #right ifTrue: [ newDirection := #down ].
					guardDirection = #down ifTrue: [ newDirection := #left ].
					guardDirection = #left ifTrue: [ newDirection := #up ].
					guard direction: newDirection ] ] repeat
]
