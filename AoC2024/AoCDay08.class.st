"
Day08 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay08',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay08 >> euclideanDistanceIntegersFrom: thisAntennaLoc to: otherAntennaLoc [

	| x1 x2 y1 y2 |
	x1 := thisAntennaLoc x.
	y1 := thisAntennaLoc y.
	x2 := otherAntennaLoc x.
	y2 := otherAntennaLoc y.

	^ ((x2 - x1) squared + (y2 - y1) squared) sqrt rounded
]

{ #category : 'parsing' }
AoCDay08 >> parse: lines [

	| map antennas height width |
	height := lines size.
	width := lines first size.

	map := CTNewArray2D width: width height: height.
	antennas := OrderedCollection new.

	lines doWithIndex: [ :row :r |
			row doWithIndex: [ :col :c |
					| spot |
					spot := c @ r.
					col ~= $. ifTrue: [
							antennas add: {
									spot.
									col } ].
					map atColumn: c atRow: r put: { col } ] ].

	^ {
		  map.
		  antennas.
		  width.
		  height }
]

{ #category : 'executing' }
AoCDay08 >> part1: lines [

	| data map antennas width height allAntinodes |
	data := self parse: lines.
	map := data first.
	antennas := data second.
	width := data third.
	height := data fourth.
	allAntinodes := OrderedCollection new.

	antennas copy do: [ :ua |
			| thisAntenna thisAntennaLoc otherAntennas |
			thisAntenna := ua second.
			thisAntennaLoc := ua first.
			otherAntennas := antennas select: [ :oa |
				                 oa second = thisAntenna and:
					                 oa first ~= thisAntennaLoc ].

			otherAntennas do: [ :oa |
					| otherAntennaLoc distance newPoints antinodes |
					otherAntennaLoc := oa first.
					distance := self
						            euclideanDistanceIntegersFrom: thisAntennaLoc
						            to: otherAntennaLoc.
					newPoints := self
						             plotNewPointsFrom: thisAntennaLoc
						             to: otherAntennaLoc
						             distance: distance.

					antinodes := newPoints select: [ :point |
						             point isWithinWidth: width height: height ].
					allAntinodes addAll: antinodes ] ].

	^ allAntinodes asSet size
]

{ #category : 'executing' }
AoCDay08 >> plotNewPointsFrom: thisAntennaLoc to: otherAntennaLoc distance: distance [

	| x1 x2 y1 y2 dx dy vecLength unitDx unitDy newPointA newPointB |
	x1 := thisAntennaLoc x.
	y1 := thisAntennaLoc y.
	x2 := otherAntennaLoc x.
	y2 := otherAntennaLoc y.
	dx := x2 - x1.
	dy := y2 - y1.
	vecLength := (dx squared + dy squared) sqrt.
	unitDx := dx / vecLength.
	unitDy := dy / vecLength.

	newPointA := (x1 - (distance * unitDx)) rounded
	             @ (y1 - (distance * unitDy)) rounded.

	newPointB := (x2 + (distance * unitDx)) rounded
	             @ (y2 + (distance * unitDy)) rounded.

	^ {
		  newPointA.
		  newPointB }
]
