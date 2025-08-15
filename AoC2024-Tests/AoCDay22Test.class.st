Class {
	#name : 'AoCDay22Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay22Test >> setUp [
	super setUp.
	self day: '22'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay22 new.
]

{ #category : 'tests' }
AoCDay22Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 13185239446
]

{ #category : 'tests' }
AoCDay22Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 37327623
]
