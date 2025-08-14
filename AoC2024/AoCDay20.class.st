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
AoCDay20 >> calculateTimeSavings: endDistances withPairs: pairs minimumSavings: minSave [

	| timeSavings timeSaved |
	timeSavings := 0.

	pairs do: [ :pair |
			| d1 d2 p1 p2 dist |
			p1 := pair first.
			p2 := pair second.
			dist := pair third.

			d1 := endDistances at: p1.
			d2 := endDistances at: p2.
			timeSaved := (d1 - d2) abs - dist.

			timeSaved >= minSave ifTrue: [ timeSavings := timeSavings + 1 ] ].

	^ timeSavings
]

{ #category : 'as yet unclassified' }
AoCDay20 >> findDistances: paths startPoint: aPoint [
	"BFS"

	| distances queue |
	distances := Dictionary with: aPoint -> 0.

	queue := OrderedCollection with: aPoint.

	[ queue isNotEmpty ] whileTrue: [
			| point currentDistance |
			point := queue removeFirst.
			currentDistance := distances at: point.

			point fourNeighbors do: [ :n |
					((paths includes: n) and: (distances includesKey: n) not)
						ifTrue: [
								distances at: n put: currentDistance + 1.
								queue add: n ] ] ].

	^ distances
]

{ #category : 'as yet unclassified' }
AoCDay20 >> findPairs: points withinDistance: threshold [

	| pairs sortedPoints numPoints |
	pairs := OrderedCollection new.

	points ifEmpty: [ ^ pairs ].

	sortedPoints := points sorted: [ :a :b | a x < b x ].
	numPoints := points size.

	1 to: numPoints do: [ :i |
			| p1 j |
			p1 := sortedPoints at: i.
			j := i + 1.

			[
			j <= numPoints and: [ (sortedPoints at: j) x - p1 x <= threshold ] ]
				whileTrue: [
						| p2 yDist |
						p2 := sortedPoints at: j.

						yDist := (p2 y - p1 y) abs.

						yDist <= threshold ifTrue: [
								| dist |
								dist := yDist + (p2 x - p1 x) abs.
								(dist between: 1 and: threshold) ifTrue: [
										pairs add: (OrderedCollection withAll: {
													 p1.
													 p2.
													 dist }) ] ].
						j := j + 1 ] ].

	^ pairs
]

{ #category : 'as yet unclassified' }
AoCDay20 >> findPaths [

	| paths |
	paths := OrderedCollection new.

	map withIndexesDo: [ :x :y :value |
		value ~= '#' ifTrue: [ paths add: x @ y ] ].

	^ paths asSet
]

{ #category : 'executing' }
AoCDay20 >> part1: lines [

	| endPoint paths endDistances pairs totalSavings |
	map := self parseToGridAsStrings: lines.

	endPoint := self find: 'E' inGrid: map.
	paths := self findPaths.

	endDistances := self findDistances: paths startPoint: endPoint.

	pairs := self findPairs: paths withinDistance: 2.

	totalSavings := self
		                calculateTimeSavings: endDistances
		                withPairs: pairs
		                minimumSavings: 100.

	^ totalSavings
]
