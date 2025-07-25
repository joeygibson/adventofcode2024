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
	map := CTNewArray2D width: lines first size height: lines size.

	lines doWithIndex: [ :line :row |
			line asArray doWithIndex: [ :c :col |
					| thing |
					thing := self createThing: c.
					c = $^ ifTrue: [
							thing col: col.
							thing row: row.
							guard := thing deepCopy.
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

	| map guard parts guardCol guardRow loopCount guardPos |
	parts := self createMapAndGuard: lines.
	map := parts first deepCopy.
	guard := parts second.
	guardCol := guard col.
	guardRow := guard row.
	guardPos := guardCol @ guardRow.
	loopCount := 0.

	map withIndexesDo: [ :col :row :thing |
			| mapCopy guardCopy results |
			parts := self createMapAndGuard: lines.
			mapCopy := parts first.
			guardCopy := parts second.


			(col @ row ~= guardPos and: (thing isKindOf: FloorTile)) ifTrue: [
					mapCopy atColumn: col atRow: row put: (self createThing: $O).

					results := self play: mapCopy withGuard: guardCopy.
					results = #InALoop ifTrue: [ loopCount := loopCount + 1 ] ] ].

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
