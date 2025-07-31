Class {
	#name : 'AoCDay14Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay14Test >> setUp [
	super setUp.
	self day: '14'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay14 new.
]

{ #category : 'tests' }
AoCDay14Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines which: #real.

	self assert: res equals: 231782040
]

{ #category : 'tests' }
AoCDay14Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines which: #sample.

	self assert: res equals: 12
]

{ #category : 'tests' }
AoCDay14Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 6475
]
