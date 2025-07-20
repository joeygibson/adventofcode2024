"
Day01 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay01',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay01 >> part1: lines [

	| diffs lists list0 list1 |
	lists := self splitAndSortLines: lines.
	list0 := lists first.
	list1 := lists second.
	
	diffs := list0 with: list1 collect: [ :i0 :i1 | (i0 - i1) abs ].

	^ diffs sum
]

{ #category : 'executing' }
AoCDay01 >> part2: lines [

	| lists list0 list1 |
	lists := self splitAndSortLines: lines.
	list0 := lists first.
	list1 := lists second.

	^ (list0 collect: [ :num | num * (list1 occurrencesOf: num) ]) sum
]

{ #category : 'utilities' }
AoCDay01 >> splitAndSortLines: lines [

	| splitLines list0 list1 |
	list0 := OrderedCollection new.
	list1 := OrderedCollection new.

	splitLines := lines collect: [ :line | line substrings ].
	splitLines do: [ :line |
			list0 add: line first asInteger.
			list1 add: line last asInteger ].

	list0 sort.
	list1 sort.

	^ {
		  list0.
		  list1 }
]
