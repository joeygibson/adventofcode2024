"
Day18 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay18',
	#superclass : 'AoCBase',
	#instVars : [
		'startPoint',
		'endPoint',
		'map'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay18 >> findShortestPath [
	| openSet closedSet cameFrom gScore fScore current neighbors |
	
	openSet := OrderedCollection with: startPoint.
	closedSet := Set new.
	cameFrom := Dictionary new.
	gScore := Dictionary new.
	fScore := Dictionary new.
	
	"Initialize scores"
	gScore at: startPoint put: 0.
	fScore at: startPoint put: (startPoint manhattanDistanceTo: endPoint).
	
	[ openSet isNotEmpty ] whileTrue: [
		"Find node in openSet with lowest fScore"
		current := openSet detectMin: [ :node | 
			fScore at: node ifAbsent: [ Float infinity ] 
		].
		
		"Check if we reached the goal"
		current = endPoint ifTrue: [
			"Reconstruct and return the path"
			^ self reconstructPath: cameFrom from: current
		].
		
		"Move current from open to closed set"
		openSet remove: current.
		closedSet add: current.
		
		"Examine each neighbor"
		neighbors := self neighborsOf: current.
		neighbors do: [ :neighbor |
			"Skip if already evaluated"
			(closedSet includes: neighbor) ifFalse: [
				| tentativeGScore |
				tentativeGScore := (gScore at: current) + 1.
				
				"Add to open set if not already there"
				(openSet includes: neighbor) ifFalse: [
					openSet add: neighbor
				].
				
				"Check if this path is better"
				tentativeGScore < (gScore at: neighbor ifAbsent: [ Float infinity ]) ifTrue: [
					"This path is the best so far, record it"
					cameFrom at: neighbor put: current.
					gScore at: neighbor put: tentativeGScore.
					fScore at: neighbor put: tentativeGScore + (neighbor manhattanDistanceTo: endPoint)
				]
			]
		]
	].
	
	"No path found"
	^ OrderedCollection new
]

{ #category : 'testing' }
AoCDay18 >> isWalkable: aPoint [

	| x y |
	x := aPoint x.
	y := aPoint y.

	"Check bounds"
	(x < 1 or: [ x > (map width) ]) ifTrue: [ ^ false ].
	(y < 1 or: [ y > map height ]) ifTrue: [ ^ false ].

	"Check if it's a wall"
	^ (map atColumn: x atRow: y) ~= '#'
]

{ #category : 'as yet unclassified' }
AoCDay18 >> neighborsOf: aPoint [

	^ aPoint fourNeighbors select: [ :neighbor |
		  self isWalkable: neighbor ]
]

{ #category : 'parsing' }
AoCDay18 >> parseToPoints: lines [

	^ lines collect: [ :line |
			  | chunks |
			  chunks := line substrings: ','.

			  chunks first asInteger + 1 @ (chunks second asInteger + 1) ]
]

{ #category : 'as yet unclassified' }
AoCDay18 >> part1: lines size: size iterations: iterations [

	| upcoming path |
	map := CTArray2D
		       width: size + 1
		       height: size + 1
		       tabulate: [ :x :y | '.' ].

	startPoint := 1 @ 1.
	endPoint := size + 1 @ (size + 1).
	upcoming := self parseToPoints: lines.

	(upcoming takeFirst: iterations) do: [ :spot | map at: spot put: '#' ].

	path := self findShortestPath.

	^ path size - 1
]

{ #category : 'as yet unclassified' }
AoCDay18 >> part2: lines size: size iterations: iterations [

	| upcoming path |
	map := CTArray2D
		       width: size + 1
		       height: size + 1
		       tabulate: [ :x :y | '.' ].

	startPoint := 1 @ 1.
	endPoint := size + 1 @ (size + 1).
	upcoming := self parseToPoints: lines.

	(upcoming takeFirst: iterations) do: [ :spot | map at: spot put: '#' ].

	path := self findShortestPath.

	(upcoming allButFirst: iterations) do: [ :spot |
			map at: spot put: '#'.
			path := self findShortestPath.

			path ifEmpty: [ ^ spot - 1 ] ]
]

{ #category : 'as yet unclassified' }
AoCDay18 >> reconstructPath: cameFrom from: current [

	| path cur |
	path := OrderedCollection new.
	cur := current.

	[ cur isNotNil ] whileTrue: [
			path addFirst: cur.
			cur := cameFrom at: cur ifAbsent: [ nil ] ].

	^ path
]

{ #category : 'as yet unclassified' }
AoCDay18 >> visualizeWithPath: path [
	| result |
	result := String streamContents: [ :stream |
		1 to: map height do: [ :row |
			1 to: map width do: [ :col |
				| point char |
				point := col @ row.
				char := map atColumn: col atRow: row.
				
				"Mark path with * (except start and end)"
				(path includes: point) ifTrue: [
					(point = startPoint or: [ point = endPoint ]) 
						ifFalse: [ char := $* ]
				].
				
				stream nextPut: char
			].
			stream cr
		]
	].
	^ result
]
