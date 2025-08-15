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
