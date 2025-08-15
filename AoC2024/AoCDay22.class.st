"
Day22 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay22',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay22 >> part1: lines [

	| results |
	results := lines collect: [ :line |
			           | secret |
			           secret := SecretNumber with: line asInteger.

			           1 to: 2000 do: [ :i | secret evolve ].

			           secret currentValue ].

	^ results sum
]

{ #category : 'executing' }
AoCDay22 >> part2: lines [

	| totalBananas |
	totalBananas := Dictionary new.

	lines do: [ :line |
			| secret seen diffs num |
			num := line asInteger.
			seen := Set new.
			diffs := OrderedCollection new.

			secret := SecretNumber with: num.

			1 to: 2000 do: [ :i |
					| nextNum diff sequence |
					nextNum := secret evolve.
					diff := nextNum \\ 10 - (num \\ 10).
					diffs add: diff.
					num := nextNum.

					i >= 4 ifTrue: [
							sequence := diffs asArray.
							(seen includes: sequence) ifFalse: [
									totalBananas
										at: sequence
										put:
										(totalBananas at: sequence ifAbsent: [ 0 ]) + (num \\ 10).
									seen add: sequence ].
							diffs removeFirst ] ] ].

	^ totalBananas values max
]
