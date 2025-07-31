"
Day13 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay13',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay13 >> cramer [

	| a b prize wa wb |
	a := self a.
	b := self b.
	prize := self prize.

	wa := prize x * b y - (prize y * b x) // (a x * b y - (a y * b x)).
	wb := a x * prize y - (a y * prize x) // (a x * b y - (a y * b x)).

	^ wa * a x + (wb * b x) = prize x
		  and: wa * a y + (wb * b y) = prize y
		  ifTrue: [ wa * 3 + wb ]
		  ifFalse: [ 0 ]
]

{ #category : 'as yet unclassified' }
AoCDay13 >> createClawMachineWithA: a andB: b andPrize: prize andOffset: offset [

	| aPoint bPoint prizePoint |
	aPoint := a first @ a second.
	bPoint := b first @ b second.
	prizePoint := prize first @ prize second + (offset @ offset).

	^ Claw withA: aPoint andB: bPoint andPrize: prizePoint
]

{ #category : 'as yet unclassified' }
AoCDay13 >> createClawMachines: lines forPart: part [

	| specs |
	specs := lines splitOn: [ :e | e isEmpty ].

	^ specs collect: [ :section |
			  | a b prize offset |
			  a := section first
				       regex: '\d+'
				       matchesCollect: [ :each | each asInteger ].
			  b := section second
				       regex: '\d+'
				       matchesCollect: [ :each | each asInteger ].
			  prize := section third
				           regex: '\d+'
				           matchesCollect: [ :each | each asInteger ].
			  offset := part = #part1
				            ifTrue: [ 0 ]
				            ifFalse: [ 10000000000000 ].

			  self
				  createClawMachineWithA: a
				  andB: b
				  andPrize: prize
				  andOffset: offset ]
]

{ #category : 'executing' }
AoCDay13 >> part1: lines [

	| machines results |
	machines := self createClawMachines: lines forPart: #part1.

	results := machines collect: [ :machine | machine cramer ].

	^ (results select: #isNotNil) sum
]

{ #category : 'executing' }
AoCDay13 >> part2: lines [

	| machines results |
	machines := self createClawMachines: lines forPart: #part2.

	results := machines collect: [ :machine | machine cramer ].

	^ (results select: #isNotNil) sum
]
