Class {
	#name : 'AoCDay03Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay03Test >> setUp [
	super setUp.
	self day: '03'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay03 new.
]

{ #category : 'tests' }
AoCDay03Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 167650499.
]

{ #category : 'tests' }
AoCDay03Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 161
]

{ #category : 'tests' }
AoCDay03Test >> testPart2WithRealData [

	| lines res combined |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.
		
	combined := lines joinUsing: ''.

	res := inst part2: (OrderedCollection with: combined).

	self assert: res equals: 95846796.
]

{ #category : 'tests' }
AoCDay03Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 48.
]
