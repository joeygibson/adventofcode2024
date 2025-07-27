Class {
	#name : 'AoCDay09Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay09Test >> setUp [
	super setUp.
	self day: '09'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay09 new.
]

{ #category : 'tests' }
AoCDay09Test >> testIsCompact [

	| s |
	s := OrderedCollection withAll: #('1' '2' '3' '.' '.' '.').
	self assert: (inst isCompact: s).

	s := OrderedCollection withAll: #('1' '2' '3' '.' '.' '.' '4' '5' '6').
	self deny: (inst isCompact: s)
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 60
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self timeLimit: (Duration seconds: 600).

	res := inst part1: lines.

	self assert: res equals: 6353658451014
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 1928
]
