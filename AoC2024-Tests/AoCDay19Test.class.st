Class {
	#name : 'AoCDay19Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay19Test >> setUp [
	super setUp.
	self day: '19'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay19 new.
]

{ #category : 'tests' }
AoCDay19Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self increaseTimeout: 600.
	res := inst part1: lines.

	self assert: res equals: 336
]

{ #category : 'tests' }
AoCDay19Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 6
]

{ #category : 'tests' }
AoCDay19Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self increaseTimeout: 600.
	
	res := inst part2: lines.

	self assert: res equals: 758890600222015
]

{ #category : 'tests' }
AoCDay19Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 16
]
