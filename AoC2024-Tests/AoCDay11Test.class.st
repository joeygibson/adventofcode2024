Class {
	#name : 'AoCDay11Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay11Test >> setUp [
	super setUp.
	self day: '11'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay11 new.
]

{ #category : 'tests' }
AoCDay11Test >> testPart1WithOtherSample [

	| lines res stones |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	stones := lines first substrings asArray collect: [ :stone |
		          stone asString asInteger ].

	res := inst blink: stones times: 1.

	self assert: res size equals: 7
]

{ #category : 'tests' }
AoCDay11Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 220999
]

{ #category : 'tests' }
AoCDay11Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 55312
]
