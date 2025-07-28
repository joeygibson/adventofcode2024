"
Day10 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay10',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay10 >> discoverTrails: map withStarts: starts [

	| score rating |
	score := 0.
	rating := 0.
	
	starts do: [ :start |
			| ends |
			ends := Dictionary new.
			self findPaths: map fromPos: start toEnds: ends.
			score := score + ends size.
			ends valuesDo: [ :value | rating := rating + value ] ].

	^ {
		  score.
		  rating }
]

{ #category : 'as yet unclassified' }
AoCDay10 >> findPaths: map fromPos: pos toEnds: ends [

	| current height width |
	height := map height.
	width := map width.

	current := map atColumn: pos x atRow: pos y.

	current = 9
		ifTrue: [ ends at: pos update: [ :v | v + 1 ] initial: 1 ]
		ifFalse: [
				(self getNeighbors: pos) do: [ :neighbor |
						| nv |
						((neighbor x between: 1 and: width) and:
							 (neighbor y between: 1 and: height)) ifTrue: [
								nv := map atColumn: neighbor x atRow: neighbor y.
								(nv isNotNil and: nv = (current + 1)) ifTrue: [
									self findPaths: map fromPos: neighbor toEnds: ends ] ] ] ]
]

{ #category : 'as yet unclassified' }
AoCDay10 >> findStarts: map [

	| starts |
	starts := OrderedCollection new.

	map withIndexesDo: [ :c :r :value |
		value = 0 ifTrue: [ starts add: c @ r ] ].

	^ starts
]

{ #category : 'executing' }
AoCDay10 >> getNeighbors: pos [

	| r c |
	c := pos x.
	r := pos y.

	^ {
		  (c @ (r - 1)).
		  (c @ (r + 1)).
		  (c - 1 @ r).
		  (c + 1 @ r) }
]

{ #category : 'parsing' }
AoCDay10 >> parse: lines [

	| map height width |
	height := lines size.
	width := lines first size.

	map := CTNewArray2D width: width height: height.

	lines doWithIndex: [ :row :r |
			row doWithIndex: [ :col :c |
				map atColumn: c atRow: r put: col asString asInteger ] ].

	^ map
]

{ #category : 'executing' }
AoCDay10 >> part1: lines [
	|map starts results|
	
	map := self parse: lines.
	starts := self findStarts: map.
	
	results := self discoverTrails: map withStarts: starts.
	
	^ results first.
	
]

{ #category : 'executing' }
AoCDay10 >> part2: lines [
	|map starts results|
	
	map := self parse: lines.
	starts := self findStarts: map.
	
	results := self discoverTrails: map withStarts: starts.
	
	^ results second.
	
]
