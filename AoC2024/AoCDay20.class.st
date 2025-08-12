"
Day20 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay20',
	#superclass : 'AoCBase',
	#instVars : [
		'map'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay20 >> findCheatableWalls: path [

	| walls |
	walls := OrderedCollection new.

	path do: [ :loc |
			loc fourNeighbors do: [ :n |
					((self isInbound: n) and: [ (map at: n) = '#' ]) ifTrue: [
							n fourNeighbors do: [ :n1 |
									((self isInbound: n1) and: [
										 n1 ~= loc and: (map at: n1) = '.' ]) ifTrue: [
										walls add: n ] ] ] ] ].

	^ walls
]

{ #category : 'testing' }
AoCDay20 >> isInbound: aPoint [

	^ (aPoint x between: 1 and: map width) and:
		  (aPoint y between: 1 and: map height)
]

{ #category : 'executing' }
AoCDay20 >> part1: lines [

	| path aStar startPoint endPoint cheatPaths savings |
	map := self parseToGridAsStrings: lines.

	startPoint := self find: 'S' inGrid: map.
	endPoint := self find: 'E' inGrid: map.

	aStar := AStarSolver
		         withMap: map
		         startPoint: startPoint
		         endPoint: endPoint.

	path := aStar findShortestPath.

	cheatPaths := (self findCheatableWalls: path) collect: [ :wall |
			              | cheatMap |
			              cheatMap := map deepCopy.
			              cheatMap at: wall put: '.'.
			              aStar map: cheatMap.
			              aStar findShortestPath ].

	savings := cheatPaths collect: [ :cheatPath |
		           path size - cheatPath size ].

	^ (savings select: [ :saving | saving > 100 ]) size
]
