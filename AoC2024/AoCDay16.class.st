"
Day16 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay16',
	#superclass : 'AoCBase',
	#instVars : [
		'maze',
		'width',
		'height',
		'startPoint',
		'endPoint'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay16 >> calculateTotalCostForPath: path [
	"Calculate total cost for a path using the same cost function as pathfinding"

	| totalCost previousDirection |
	"Handle empty or single-node paths"
	path size <= 1 ifTrue: [ ^ 0 ].

	totalCost := 0.
	"Mouse starts facing east"
	previousDirection := #east.

	"Walk through each step in the path"
	2 to: path size do: [ :i |
			| fromPoint toPoint stepCost |
			fromPoint := path at: i - 1.
			toPoint := path at: i.

			"Use the same cost function as in pathfinding"
			stepCost := self
				            costFrom: fromPoint
				            to: toPoint
				            previousDirection: previousDirection.

			totalCost := totalCost + stepCost.

			"Update direction for next iteration"
			previousDirection := self directionFrom: fromPoint to: toPoint ].

	^ totalCost
]

{ #category : 'as yet unclassified' }
AoCDay16 >> computeCostFromPrevious: previous toCurrent: current toNeighbor: neighbor [

	| dir |
	previous ifNil: [ ^ 1 ].

	dir := self
		       isStraightOrTurnFromPrevious: previous
		       toCurrent: current
		       toNeighbor: neighbor.
		
	Transcript show: dir asString; cr.

	dir = #turn
		ifTrue: [ ^ 1001 ]
		ifFalse: [ ^ 1 ]
]

{ #category : 'as yet unclassified' }
AoCDay16 >> costFrom: fromPoint to: toPoint previousDirection: prevDirection [
    "Return 1 for straight movement, 1001 for 90° turns"
	"Mouse starts facing east, so first move considers this"
	| currentDirection |
	
	"Calculate current direction"
	currentDirection := self directionFrom: fromPoint to: toPoint.
	
	"Previous direction should never be nil now since we start facing east"
	prevDirection isNil ifTrue: [ 
		"This shouldn't happen, but if it does, assume facing east"
		^ currentDirection = #east ifTrue: [ 1 ] ifFalse: [ 1001 ]
	].
	
	"If continuing in same direction, low cost"
	currentDirection = prevDirection ifTrue: [ ^ 1 ].
	
	"If direction changed, high cost (90° turn)"
	^ 1001
]

{ #category : 'as yet unclassified' }
AoCDay16 >> directionFrom: fromPoint to: toPoint [
	"Return direction symbol for cardinal movement"
	| dx dy |
	dx := toPoint x - fromPoint x.
	dy := toPoint y - fromPoint y.
	
	dx = 0 ifTrue: [
		^ dy > 0 ifTrue: [ #south ] ifFalse: [ #north ]
	].
	
	dy = 0 ifTrue: [
		^ dx > 0 ifTrue: [ #east ] ifFalse: [ #west ]
	].
	
	"Shouldn't happen in cardinal-only movement"
	^ #unknown
]

{ #category : 'as yet unclassified' }
AoCDay16 >> findShortestPath [

	| openSet closedSet cameFrom gScore fScore current neighbors directionTo |
	"Each node is now represented as point@direction""Mouse starts facing east, so initial state is startPoint -> #east"
	openSet := OrderedCollection with: startPoint -> #east.
	closedSet := Set new.
	cameFrom := Dictionary new.
	gScore := Dictionary new.
	fScore := Dictionary new.
	directionTo := Dictionary new. "Track which direction we arrived from"

	"Initialize scores for start node (facing east)"
	gScore at: startPoint -> #east put: 0.
	fScore
		at: startPoint -> #east
		put: (self manhattanDistanceFrom: startPoint to: endPoint).

	[ openSet isNotEmpty ] whileTrue: [ "Find node in openSet with lowest fScore"
			current := openSet detectMin: [ :node |
				           fScore at: node ifAbsent: [ Float infinity ] ].

			"Check if we reached the goal (any direction)"
			current key = endPoint ifTrue: [ "Reconstruct and return the path"
				^ self reconstructPathWithDirections: cameFrom from: current ].

			"Move current from open to closed set"
			openSet remove: current.
			closedSet add: current.

			"Examine each neighbor"
			neighbors := self neighborsOf: current key.
			neighbors do: [ :neighborPoint |
					| neighborDirection neighborNode tentativeGScore moveCost |
					"Calculate direction from current to neighbor"
					neighborDirection := self
						                     directionFrom: current key
						                     to: neighborPoint.
					neighborNode := neighborPoint -> neighborDirection.

					"Skip if already evaluated"
					(closedSet includes: neighborNode) ifFalse: [ "Calculate cost including turn penalty""current value is the direction we're currently facing"
							moveCost := self
								            costFrom: current key
								            to: neighborPoint
								            previousDirection: current value.

							tentativeGScore := (gScore at: current) + moveCost.

							"Add to open set if not already there"
							(openSet includes: neighborNode) ifFalse: [
								openSet add: neighborNode ].

							"Check if this path is better"
							tentativeGScore
							< (gScore at: neighborNode ifAbsent: [ Float infinity ])
								ifTrue: [ "This path is the best so far, record it"
										cameFrom at: neighborNode put: current.
										gScore at: neighborNode put: tentativeGScore.
										fScore at: neighborNode put: tentativeGScore
											+ (self manhattanDistanceFrom: neighborPoint to: endPoint).
										directionTo at: neighborNode put: neighborDirection ] ] ] ].

	"No path found"
	^ OrderedCollection new
]

{ #category : 'as yet unclassified' }
AoCDay16 >> findStartAndEnd [

	height := maze height.
	width := maze width.

	maze withIndexesDo: [ :c :r :v |
			v = 'S' ifTrue: [ startPoint := c @ r ].
			v = 'E' ifTrue: [ endPoint := c @ r ] ]
]

{ #category : 'testing' }
AoCDay16 >> isStraightOrTurnFromPrevious: previous toCurrent: current toNeighbor: neighbor [

	| prevDir curDir |
	prevDir := self directionFrom: previous to: current.
	curDir := self directionFrom: current to: neighbor.

	(prevDir isNil or: curDir isNil) ifTrue: [ ^ #invalid ].

	prevDir = curDir
		ifTrue: [ ^ #straight ]
		ifFalse: [ ^ #turn ]
]

{ #category : 'testing' }
AoCDay16 >> isWalkable: aPoint [

	| x y |
	x := aPoint x.
	y := aPoint y.

	"Check bounds"
	(x < 1 or: [ x > width ]) ifTrue: [ ^ false ].
	(y < 1 or: [ y > height ]) ifTrue: [ ^ false ].

	"Check if it's a wall"
	^ (maze atColumn: x atRow: y) ~= '#'
]

{ #category : 'as yet unclassified' }
AoCDay16 >> manhattanDistanceFrom: a to: b [

	^ (a x - b x) abs + (a y - b y) abs
]

{ #category : 'as yet unclassified' }
AoCDay16 >> neighborsOf: aPoint [

	^ aPoint fourNeighbors select: [ :neighbor |
		  self isWalkable: neighbor ]
]

{ #category : 'parsing' }
AoCDay16 >> parseToGrid: lines withConverter: aBlock [

	| map |
	height := lines size.
	width := lines first size.

	map := CTNewArray2DRowsAndColumns width: width height: height.

	lines doWithIndex: [ :row :r |
			row doWithIndex: [ :col :c |
				map atColumn: c atRow: r put: (aBlock value: col) ] ].

	^ map
]

{ #category : 'executing' }
AoCDay16 >> part1: lines [

	| path |
	maze := self parseToGridAsStrings: lines.
	self findStartAndEnd.

	path := self findShortestPath.

	^ self calculateTotalCostForPath: path
]

{ #category : 'as yet unclassified' }
AoCDay16 >> reconstructPathWithDirections: cameFrom from: currentNode [
	"Reconstruct path from node->direction pairs back to just points"
	| path current |
	path := OrderedCollection new.
	current := currentNode.
	
	[ current isNotNil ] whileTrue: [
		path addFirst: current key.  "Extract just the point from point->direction"
		current := cameFrom at: current ifAbsent: [ nil ]
	].
	
	^ path
]
