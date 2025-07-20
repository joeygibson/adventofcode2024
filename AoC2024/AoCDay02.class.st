Class {
	#name : 'AoCDay02',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay02 >> allAscendingOrDescending: line [

	| ascending descending numbers |
	numbers := line substrings collect: [ :str | str asInteger ].

	ascending := numbers overlappingPairsCollect: [ :first :second |
		             first < second ].

	descending := numbers overlappingPairsCollect: [ :first :second |
		              first > second ].

	^ (ascending allSatisfy: [ :item | item ]) or:
		  (descending allSatisfy: [ :item | item ])
]

{ #category : 'executing' }
AoCDay02 >> graduallyChanging: line [

	| numbers graduallyIncreasing |
	numbers := line substrings collect: [ :str | str asInteger ].

	graduallyIncreasing := numbers overlappingPairsCollect: [
			                       :first
			                       :second |
			                       | diff |
			                       diff := (first - second) abs.

			                       diff > 0 and: diff < 4 ].

	^ graduallyIncreasing allSatisfy: [ :diff | diff ]
]

{ #category : 'executing' }
AoCDay02 >> part1: lines [
	|safe|
	
	safe := lines select: [ :line |
		(self allAscendingOrDescending: line ) and:
			(self graduallyChanging: line )].
		
	^ safe size.
	
]
