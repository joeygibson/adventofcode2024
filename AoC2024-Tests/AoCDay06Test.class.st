Class {
	#name : 'AoCDay06Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay06Test >> setUp [
	super setUp.
	self day: '06'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay06 new.
]

{ #category : 'tests' }
AoCDay06Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 5269
]

{ #category : 'tests' }
AoCDay06Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 41
]

{ #category : 'tests' }
AoCDay06Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self timeLimit: (Duration seconds: 600).

	res := inst part2: lines.

	self assert: res equals: 1957
]

{ #category : 'tests' }
AoCDay06Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 6
]
