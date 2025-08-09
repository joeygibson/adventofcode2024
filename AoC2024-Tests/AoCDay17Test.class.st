Class {
	#name : 'AoCDay17Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay17Test >> setUp [
	super setUp.
	self day: '17'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay17 new.
]

{ #category : 'tests' }
AoCDay17Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: '2,3,6,2,1,6,1,2,1'
]

{ #category : 'tests' }
AoCDay17Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: '4,6,3,5,6,3,5,2,1,0'
]

{ #category : 'tests' }
AoCDay17Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.
		
	res := inst part2: lines.

	self assert: res equals: 90938893795561
]

{ #category : 'tests' }
AoCDay17Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'foo.txt'
		         for: self day
		         onPath: self path.
		
	res := inst part2: lines.

	self assert: res equals: 117440
]
