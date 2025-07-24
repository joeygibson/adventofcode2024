"
Day02 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay02',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay02 >> allAscendingOrDescending: numbers [

	| ascending descending  |
	ascending := numbers overlappingPairsCollect: [ :first :second |
		             first < second ].

	descending := numbers overlappingPairsCollect: [ :first :second |
		              first > second ].

	^ (ascending allSatisfy: [ :item | item ]) or:
		  (descending allSatisfy: [ :item | item ])
]

{ #category : 'executing' }
AoCDay02 >> checkReport: numbers [

	^ (self allAscendingOrDescending: numbers) and:
		  (self graduallyChanging: numbers)
]

{ #category : 'executing' }
AoCDay02 >> graduallyChanging: numbers [

	| graduallyIncreasing |
	graduallyIncreasing := numbers overlappingPairsCollect: [
			                       :first
			                       :second |
			                       | diff |
			                       diff := (first - second) abs.

			                       diff > 0 and: diff < 4 ].

	^ graduallyIncreasing allSatisfy: [ :diff | diff ]
]

{ #category : 'testing' }
AoCDay02 >> isSafeWithDampening: numbers removingItem: itemNumber [

	| amendedReport |
	itemNumber > numbers size ifTrue: [ ^ false ].

	amendedReport := numbers copyWithoutIndex: itemNumber.

	^ (self checkReport: amendedReport)
		  ifTrue: [ ^ true ]
		  ifFalse: [
		  self isSafeWithDampening: numbers removingItem: itemNumber + 1 ]
]

{ #category : 'executing' }
AoCDay02 >> part1: lines [
	|safe|
	
	safe := lines select: [ :line |
		|numbers|
		numbers := line substrings collect: [ :str | str asInteger ].
		self checkReport: numbers].
		
	^ safe size.
	
]

{ #category : 'executing' }
AoCDay02 >> part2: lines [

	| safe unsafe dampenedGood |
	safe := lines select: [ :line |
			        | numbers |
			        numbers := line substrings collect: [ :str | str asInteger ].
			        (self allAscendingOrDescending: numbers) and:
				        (self graduallyChanging: numbers) ].

	unsafe := lines reject: [ :line |
			          | numbers |
			          numbers := line substrings collect: [ :str |
				                     str asInteger ].
			          (self allAscendingOrDescending: numbers) and:
				          (self graduallyChanging: numbers) ].

	dampenedGood := unsafe select: [ :line |
			                | numbers |
			                numbers := line substrings collect: [ :str |
				                           str asInteger ].
			                self isSafeWithDampening: numbers removingItem: 1 ].

	^ safe size + dampenedGood size
]
