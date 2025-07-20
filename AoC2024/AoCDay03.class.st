Class {
	#name : 'AoCDay03',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay03 >> part1: lines [

	| results |
	results := lines collect: [ :line | self processLine: line ].

	^ results sum
]

{ #category : 'as yet unclassified' }
AoCDay03 >> processLine: aLine [

	| regex matches res |
	regex := '(mul\(\d+,\d+\))' asRegex.
	matches := regex matchesIn: aLine collect: [ :c | c ].

	res := matches collect: [ :m |
			  | numbers |
			  numbers := '\d+' asRegex matchesIn: m.

			  numbers inject: 1 into: [ :sum :num | sum * num asInteger ] ].
		
	^ res sum.
]
