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

{ #category : 'parsing' }
AoCDay18 >> parseToPoints: lines [

	^ lines collect: [ :line |
			  | chunks |
			  chunks := line substrings: ','.

			  chunks first asInteger + 1 @ (chunks second asInteger + 1) ]
]

{ #category : 'as yet unclassified' }
AoCDay18 >> part1: lines size: size iterations: iterations [

	| upcoming path aStar |
	map := CTArray2D
		       width: size + 1
		       height: size + 1
		       tabulate: [ :x :y | '.' ].

	startPoint := 1 @ 1.
	endPoint := size + 1 @ (size + 1).

	aStar := AStarSolver
		         withMap: map
		         startPoint: startPoint
		         endPoint: endPoint.

	upcoming := self parseToPoints: lines.

	(upcoming takeFirst: iterations) do: [ :spot | map at: spot put: '#' ].

	path := aStar findShortestPath.

	^ path size - 1
]

{ #category : 'as yet unclassified' }
AoCDay18 >> part2: lines size: size iterations: iterations [

	| upcoming path aStar |
	map := CTArray2D
		       width: size + 1
		       height: size + 1
		       tabulate: [ :x :y | '.' ].

	startPoint := 1 @ 1.
	endPoint := size + 1 @ (size + 1).

	aStar := AStarSolver
		         withMap: map
		         startPoint: startPoint
		         endPoint: endPoint.

	upcoming := self parseToPoints: lines.

	(upcoming takeFirst: iterations) do: [ :spot | map at: spot put: '#' ].

	path := aStar findShortestPath.

	(upcoming allButFirst: iterations) do: [ :spot |
			map at: spot put: '#'.
			aStar map: map.

			path := aStar findShortestPath.

			path ifEmpty: [ ^ spot - 1 ] ]
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
