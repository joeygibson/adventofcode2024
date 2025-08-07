"
Day16 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay16',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay16 >> aStarFrom: start to: goal on: maze [

	| openSet closedSet cameFrom gScore fScore previous |
	openSet := OrderedCollection with:
		           (SpotAndScore withSpot: start andScore: 0).
	closedSet := Dictionary new.
	cameFrom := Dictionary new.
	gScore := Dictionary with: start -> 0.
	fScore := Dictionary with:
		          start -> (self manhattanDistanceFrom: start to: goal).
	previous := start - (0 @ 1).

	[ openSet isEmpty ] whileFalse: [
			| currentPair current |
			currentPair := openSet detectMin: [ :item | item score ].
			current := currentPair spot.
			
			current = goal ifTrue: [ 
				^ self reconstructPath: cameFrom current: current.
				 ]
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			 ].
]

{ #category : 'as yet unclassified' }
AoCDay16 >> findStartAndEnd: maze [

	| start end |
	start := nil.
	end := nil.
	
	maze withIndexesDo: [ :c :r :v |
			v = 'S' ifTrue: [ start := c @ r ].
			v = 'E' ifTrue: [ end := c @ r ] ].

	^ {
		  start.
		  end }
]

{ #category : 'executing' }
AoCDay16 >> part1: lines [

	| maze data start end path |
	maze := self parseToGridAsStrings: lines.
	data := self findStartAndEnd: maze.
	start := data first.
	end := data second.

	path := self
		        aStarFrom: start
		        to: end
		        on: maze
		        withNeighbors: #neighbors
		        withCost: #computeCost
		        withDistance: #manhattanDistance
]
