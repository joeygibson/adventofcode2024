"
Day19 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay19',
	#superclass : 'AoCBase',
	#instVars : [
		'towels',
		'designs',
		'designCache'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'initialization' }
AoCDay19 >> initialize [
	designCache := Dictionary new.
]

{ #category : 'accessing' }
AoCDay19 >> isPossible: design startingAt: index [

	^ designCache
		  at: (OrderedCollection with: design with: index)
		  ifAbsentPut: [
				  | res |
				  design size = index
					  ifTrue: [ 1 ]
					  ifFalse: [
							  res := 0.

							  towels do: [ :towel |
									  | pattern |
									  pattern := '^{1}' format: { towel }.
									  (pattern asRegex matchesPrefix: (design allButFirst: index))
										  ifTrue: [
											  res := res
											         +
											         (self
												          isPossible: design
												          startingAt: towel size + index) ] ].
							  res ] ]
]

{ #category : 'executing' }
AoCDay19 >> part1: lines [

	| possibilities |
	towels := lines first substrings: ', '.
	designs := lines allButFirst: 2.

	possibilities := designs collect: [ :design |
		                 self isPossible: design startingAt: 0 ].

	^ (possibilities select: [ :item | item ~= 0 ]) size
]

{ #category : 'executing' }
AoCDay19 >> part2: lines [

	| possibilities |
	towels := lines first substrings: ', '.
	designs := lines allButFirst: 2.

	possibilities := designs collect: [ :design |
		                 self isPossible: design startingAt: 0 ].

	^ possibilities sum
]
