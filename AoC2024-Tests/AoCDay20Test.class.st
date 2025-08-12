Class {
	#name : 'AoCDay20Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay20Test >> setUp [
	super setUp.
	self day: '20'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay20 new.
]

{ #category : 'tests' }
AoCDay20Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self increaseTimeout: 600.
	res := inst part1: lines.

	self assert: res equals: 1530
]

{ #category : 'tests' }
AoCDay20Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 6
]
