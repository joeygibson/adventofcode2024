Class {
	#name : 'AoCDay13Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay13Test >> setUp [
	super setUp.
	self day: '13'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay13 new.
]

{ #category : 'tests' }
AoCDay13Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 29522
]

{ #category : 'tests' }
AoCDay13Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 480
]

{ #category : 'tests' }
AoCDay13Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 101214869433312
]

{ #category : 'tests' }
AoCDay13Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 875318608908
]
