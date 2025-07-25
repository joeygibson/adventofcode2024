Class {
	#name : 'AoCDay08Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay08Test >> setUp [
	super setUp.
	self day: '08'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay08 new.
]

{ #category : 'tests' }
AoCDay08Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 2
]

{ #category : 'tests' }
AoCDay08Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 364
]

{ #category : 'tests' }
AoCDay08Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 14
]
