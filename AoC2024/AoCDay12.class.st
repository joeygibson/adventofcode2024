"
Day12 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay12',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay12 >> buildFences: garden withWidth: width andHeight: height [

	| regions sides keys |
	regions := OrderedCollection new.
	sides := OrderedCollection new.
	keys := garden keys sorted.

	keys do: [ :plot |
			(self regions: regions contains: plot) ifFalse: [
					| res |
					res := self
						       floodFillMap: garden
						       fromPlot: plot
						       withWidth: width
						       andHeight: height.

					regions add: res first.
					sides add: res second ] ].

	^ (regions
		   with: sides
		   collect: [ :region :side | region size * side ]) sum
]

{ #category : 'executing' }
AoCDay12 >> floodFillMap: garden fromPlot: start withWidth: width andHeight: height [

	| originalCrop queue visited totalSides region |
	originalCrop := garden at: start.
	queue := OrderedCollection with: start.
	visited := OrderedCollection new.
	totalSides := 0.
	region := OrderedCollection with: start.

	[ queue isEmpty ] whileFalse: [
			| plot directions |
			plot := queue removeFirst.

			(visited includes: plot) ifFalse: [
					visited add: plot.
					directions := plot fourNeighbors.

					directions do: [ :dp |
							((dp isWithinWidth: width height: height) not or:
								 (garden at: dp ifAbsent: nil) ~= originalCrop) ifTrue: [
								totalSides := totalSides + 1 ].

							((dp isWithinWidth: width height: height) and:
								 ((garden at: dp ifAbsent: nil) = originalCrop and:
									  (visited includes: dp) not)) ifTrue: [
									region addIfNotPresent: dp.
									queue addLast: dp ] ] ] ].

	^ {
		  region.
		  totalSides }
]

{ #category : 'executing' }
AoCDay12 >> part1: lines [

	| garden width height |
	height := lines size.
	width := lines first size.
	
	garden := self parseToDictGridAsStrings: lines.
	
	^ self buildFences: garden withWidth: width andHeight: height.
	
	
]

{ #category : 'as yet unclassified' }
AoCDay12 >> regions: regions contains: plot [

	| found |
	found := regions
		         detect: [ :region | region includes: plot ]
		         ifNone: nil.

	^ found isNotNil
]
