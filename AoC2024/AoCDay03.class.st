"
Day03 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay03',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay03 >> part1: lines [

	| results linesWithData |
	linesWithData := lines reject: [ :line | line trim isEmpty ].
	results := linesWithData collect: [ :line | self processLine: line ].

	^ results sum
]

{ #category : 'executing' }
AoCDay03 >> part2: lines [

	| results |
	results := lines collect: [ :line | self processLineWithConditions: line ].

	^ results sum
]

{ #category : 'executing' }
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

{ #category : 'executing' }
AoCDay03 >> processLineWithConditions: aLine [

	| regex matches res enabled |
	enabled := true.
	regex := '(mul\(\d+,\d+\)|do\(\)|don''t\(\))' asRegex.
	matches := regex matchesIn: aLine collect: [ :c | c ].
	res := OrderedCollection new.

	matches do: [ :m |
			m = 'do()'
				ifTrue: [ enabled := true ]
				ifFalse: [
						m = 'don''t()'
							ifTrue: [ enabled := false ]
							ifFalse: [
									enabled ifTrue: [
											| numbers |
											numbers := '\d+' asRegex matchesIn: m.

											res add:
												(numbers
													 inject: 1
													 into: [ :sum :num | sum * num asInteger ]) ] ] ] ].

	^ res sum
]
