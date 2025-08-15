"
Day21 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay21',
	#superclass : 'AoCBase',
	#instVars : [
		'numericKeypad',
		'directionKeypad',
		'numericKeypadMap',
		'directionKeypadMap'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay21 >> createDirectionalKeypad [

	| keypad map |
	keypad := Dictionary new.
	map := CTNewArray2D width: 3 height: 4.

	map atColumn: 2 atRow: 1 put: '^'.
	map atColumn: 3 atRow: 1 put: 'A'.

	keypad at: '^' put: 2 @ 1.
	keypad at: 'A' put: 3 @ 1.

	map atColumn: 1 atRow: 2 put: '<'.
	map atColumn: 2 atRow: 2 put: 'v'.
	map atColumn: 3 atRow: 2 put: '>'.

	keypad at: '<' put: 1 @ 2.
	keypad at: 'v' put: 2 @ 2.
	keypad at: '>' put: 3 @ 2.

	directionKeypad := keypad.
	directionKeypadMap := map
]

{ #category : 'as yet unclassified' }
AoCDay21 >> createNumericKeypad [

	| keypad map |
	keypad := Dictionary new.
	map := CTNewArray2D width: 3 height: 4.

	map atColumn: 1 atRow: 1 put: '7'.
	map atColumn: 2 atRow: 1 put: '8'.
	map atColumn: 3 atRow: 1 put: '9'.

	keypad at: '7' put: 1 @ 1.
	keypad at: '8' put: 2 @ 1.
	keypad at: '9' put: 3 @ 1.

	map atColumn: 1 atRow: 2 put: '4'.
	map atColumn: 2 atRow: 2 put: '5'.
	map atColumn: 3 atRow: 2 put: '6'.

	keypad at: '4' put: 1 @ 2.
	keypad at: '5' put: 2 @ 2.
	keypad at: '6' put: 3 @ 2.

	map atColumn: 1 atRow: 3 put: '1'.
	map atColumn: 2 atRow: 3 put: '2'.
	map atColumn: 3 atRow: 3 put: '3'.

	keypad at: '1' put: 1 @ 3.
	keypad at: '2' put: 2 @ 3.
	keypad at: '3' put: 3 @ 3.

	map atColumn: 2 atRow: 4 put: '0'.
	map atColumn: 3 atRow: 4 put: 'A'.

	keypad at: '0' put: 2 @ 4.
	keypad at: 'A' put: 3 @ 4.

	numericKeypad := keypad.
	numericKeypadMap := map
]

{ #category : 'as yet unclassified' }
AoCDay21 >> enterCode: code onKeypad: keypad withMap: map [

	| keys allMoves aStar |
	keys := OrderedCollection
		        with: 'A';
		        withAll:
			        (code asOrderedCollection collect: [ :c | c asString ]).
	allMoves := OrderedCollection new.

	keys overlappingPairsDo: [ :a :b |
			| keyA keyB moves |
			keyA := keypad at: a.
			keyB := keypad at: b.

			aStar := AStarSolver
				         withMap: map
				         startPoint: keyA
				         endPoint: keyB.
			moves := aStar findShortestPath.

			allMoves add: moves ].

	^ allMoves joinUsing: 'A'
]

{ #category : 'executing' }
AoCDay21 >> part1: codes [

	| complexities |
	complexities := OrderedCollection new.

	self createNumericKeypad.
	self createDirectionalKeypad.

	codes do: [ :code |
			| codeMoves dir1Moves dir2Moves numericPortion complexity |
			codeMoves := self
				             enterCode: code
				             onKeypad: numericKeypad
				             withMap: numericKeypadMap.
			dir1Moves := self
				             enterCode: codeMoves
				             onKeypad: directionKeypad
				             withMap: directionKeypadMap.
			dir2Moves := self
				             enterCode: dir1Moves
				             onKeypad: directionKeypad
				             withMap: directionKeypadMap ]
]
