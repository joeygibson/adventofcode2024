"
Day07 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay07',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay07 >> interleave: arr1 with: arr2 [

	| res |
	res := OrderedCollection new.

	arr1 doWithIndex: [ :arg :index |
			res add: arg.
			index <= arr2 size ifTrue: [ res add: (arr2 at: index) ] ].

	^ res
]

{ #category : 'parsing' }
AoCDay07 >> parse: lines [

	^ lines collect: [ :line |
		  (line substrings: ': ') collect: [ :str | str asInteger ] ]
]

{ #category : 'executing' }
AoCDay07 >> part1: lines [

	^ self partX: lines withOperators: { $*. $+ }
]

{ #category : 'as yet unclassified' }
AoCDay07 >> partX: lines withOperators: allOperators [

	| equations solvable solved |
	equations := self parse: lines.
	solvable := OrderedCollection new.

	equations do: [ :equation |
			| answer operands operators |
			answer := equation first.
			operands := equation allButFirst.
			operators := allOperators cartesianProductOfLength:
				             operands size - 1.
			solved := operators
				          detect: [ :opSet |
						          | res |
						          res := self solve:
							                 (self interleave: operands with: opSet).
						          res = answer ]
				          ifNone: nil.
			solved ifNotNil: [ solvable add: answer ] ].

	^ solvable sum
]

{ #category : 'as yet unclassified' }
AoCDay07 >> solve: ops [

	| op1 op2 op3 remainingOps solution |
	op1 := ops first.
	op2 := ops second.
	op3 := ops third.
	remainingOps := ops allButFirst: 3.

	op2 = $* ifTrue: [ solution := op1 * op3 ].
	op2 = $+ ifTrue: [ solution := op1 + op3 ].

	remainingOps isNotEmpty
		ifTrue: [
				| arr |
				arr := OrderedCollection new.
				arr add: solution.
				arr addAll: remainingOps.
				^ self solve: arr ]
		ifFalse: [ ^ solution ]
]
