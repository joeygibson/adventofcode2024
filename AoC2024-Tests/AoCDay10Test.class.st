Class {
	#name : 'AoCDay10Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay10Test >> setUp [
	super setUp.
	self day: '10'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay10 new.
]

{ #category : 'tests' }
AoCDay10Test >> testFindStarts [

	| lines map starts |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	map := inst parse: lines.

	starts := inst findStarts: map.

	self assert: starts size equals: 1.
	self assert: starts first equals: 1 @ 1
]

{ #category : 'tests' }
AoCDay10Test >> testFindStartsWithManyResults [

	| lines map starts |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	map := inst parse: lines.

	starts := inst findStarts: map.

	self assert: starts size equals: 9.

]

{ #category : 'tests' }
AoCDay10Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 36
]

{ #category : 'tests' }
AoCDay10Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 430
]

{ #category : 'tests' }
AoCDay10Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 1
]

{ #category : 'tests' }
AoCDay10Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 928
]
