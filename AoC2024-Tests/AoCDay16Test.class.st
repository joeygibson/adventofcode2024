Class {
	#name : 'AoCDay16Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay16Test >> setUp [
	super setUp.
	self day: '16'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay16 new.
	
	Transcript clear.
]

{ #category : 'tests' }
AoCDay16Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 11048
]

{ #category : 'tests' }
AoCDay16Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 122492
]

{ #category : 'tests' }
AoCDay16Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 7036
]
