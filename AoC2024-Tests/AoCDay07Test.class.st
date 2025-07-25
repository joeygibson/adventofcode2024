Class {
	#name : 'AoCDay07Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay07Test >> setUp [
	super setUp.
	self day: '07'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay07 new.
]

{ #category : 'tests' }
AoCDay07Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 5512534574980
]

{ #category : 'tests' }
AoCDay07Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 3749
]

{ #category : 'tests' }
AoCDay07Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.
	
	self timeLimit: (Duration seconds: 600).
	
	res := inst part2: lines.

	self assert: res equals: 328790210468594
]

{ #category : 'tests' }
AoCDay07Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 11387
]
