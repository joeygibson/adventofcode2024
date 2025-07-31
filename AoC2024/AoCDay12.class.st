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
AoCDay12 >> buildFences: garden withWidth: width andHeight: height forPart: part [

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

	^ (regions with: sides collect: [ :region :side |
			   | multiplier |
			   multiplier := part = #part1
				                 ifTrue: [ side ]
				                 ifFalse: [ self countCorners: region ].
			   region size * multiplier ]) sum
]

{ #category : 'as yet unclassified' }
AoCDay12 >> countCorners: region [

	| sides |
	sides := 0.

	region do: [ :plot |
			| neighbors |
			neighbors := Dictionary new.

			plot eightNeighbors
				with: { #e. #se. #s. #sw. #w. #nw. #n. #ne }
				do: [ :n :d | neighbors at: d put: (region includes: n) ].

			((neighbors at: #n) not and: (neighbors at: #e) not) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #n) not and: (neighbors at: #w) not) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #s) not and: (neighbors at: #e) not) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #s) not and: (neighbors at: #w) not) ifTrue: [
				sides := sides + 1 ].

			((neighbors at: #n) and:
				 ((neighbors at: #e) and: (neighbors at: #ne) not)) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #n) and:
				 ((neighbors at: #w) and: (neighbors at: #nw) not)) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #s) and:
				 ((neighbors at: #e) and: (neighbors at: #se) not)) ifTrue: [
				sides := sides + 1 ].
			((neighbors at: #s) and:
				 ((neighbors at: #w) and: (neighbors at: #sw) not)) ifTrue: [
				sides := sides + 1 ] ].

	^ sides
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

	^ self
		  buildFences: garden
		  withWidth: width
		  andHeight: height
		  forPart: #part1
]

{ #category : 'executing' }
AoCDay12 >> part2: lines [

	| garden width height |
	height := lines size.
	width := lines first size.

	garden := self parseToDictGridAsStrings: lines.

	^ self
		  buildFences: garden
		  withWidth: width
		  andHeight: height
		  forPart: #part2
]

{ #category : 'as yet unclassified' }
AoCDay12 >> regions: regions contains: plot [

	| found |
	found := regions
		         detect: [ :region | region includes: plot ]
		         ifNone: nil.

	^ found isNotNil
]
