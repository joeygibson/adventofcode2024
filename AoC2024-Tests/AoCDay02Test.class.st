Class {
	#name : 'AoCDay02Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'tests' }
AoCDay02Test >> testAllAscendingOrDescending [

	| line |
	line := #( 1 2 3 4 5 ).
	self assert: (inst allAscendingOrDescending: line).

	line := #( 5 4 3 2 1 ).
	self assert: (inst allAscendingOrDescending: line).

	line := #( 1 3 2 5 4 ).
	self deny: (inst allAscendingOrDescending: line)
]

{ #category : 'tests' }
AoCDay02Test >> testGraduallyChanging [

	| line |
	line := #( 1 2 3 4 5 ).
	self assert: (inst graduallyChanging: line).

	line := #( 5 4 3 2 1 ).
	self assert: (inst allAscendingOrDescending: line).

	line := #( 1 3 2 5 4 ).
	self deny: (inst allAscendingOrDescending: line)
]

{ #category : 'tests' }
AoCDay02Test >> testPart1WithRealData [
	|lines res|
	
	lines := self readInputFileAsLines: 'input1.txt'.
	
	res := inst part1: lines.
	
	self assert: res equals: 549.
]

{ #category : 'tests' }
AoCDay02Test >> testPart1WithSample [
	|lines res|
	
	lines := self readInputFileAsLines: 'input0.txt'.
	
	res := inst part1: lines.
	
	self assert: res equals: 2.
]

{ #category : 'tests' }
AoCDay02Test >> testPart2WithRealData [
	|lines res|
	
	lines := self readInputFileAsLines: 'input1.txt'.
	
	res := inst part2: lines.
	
	self assert: res equals: 589.
]

{ #category : 'tests' }
AoCDay02Test >> testPart2WithSample [
	|lines res|
	
	lines := self readInputFileAsLines: 'input0.txt'.
	
	res := inst part2: lines.
	
	self assert: res equals: 4.
]
