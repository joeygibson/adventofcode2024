Class {
	#name : 'AoCDay18Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay18Test >> setUp [
	super setUp.
	self day: '18'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay18 new.
]

{ #category : 'tests' }
AoCDay18Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines size: 70 iterations: 1024.

	self assert: res equals: 354
]

{ #category : 'tests' }
AoCDay18Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines size: 6 iterations: 12.

	self assert: res equals: 22
]
