Class {
	#name : 'AoCDay15Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay15Test >> setUp [
	super setUp.
	self day: '15'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay15 new.
]

{ #category : 'tests' }
AoCDay15Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 2028
]

{ #category : 'tests' }
AoCDay15Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 1360570
]

{ #category : 'tests' }
AoCDay15Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 10092
]
