Class {
	#name : 'AoCDay01Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'tests' }
AoCDay01Test >> testPart1WithRealData [
	|lines res|
	
	lines := self readInputFileAsLines: 'input1.txt'.
	
	res := inst part1: lines.
	
	self assert: res equals: 3508942.
]

{ #category : 'tests' }
AoCDay01Test >> testPart1WithSample [
	|lines res|
	Transcript show: Day; cr.
	lines := self readInputFileAsLines: 'input0.txt'.
	
	res := inst part1: lines.
	
	self assert: res equals: 11.
]

{ #category : 'tests' }
AoCDay01Test >> testPart2WithRealData [
	|lines res|
	
	lines := self readInputFileAsLines: 'input1.txt'.
	
	res := inst part2: lines.
	
	self assert: res equals: 26593248.
]

{ #category : 'tests' }
AoCDay01Test >> testPart2WithSample [
	|lines res|
	
	lines := self readInputFileAsLines: 'input0.txt'.
	
	res := inst part2: lines.
	
	self assert: res equals: 31.
]
