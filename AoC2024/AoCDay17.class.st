"
Day17 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay17',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay17 >> createComputer: lines [

	| numbers |
	numbers := lines collect: [ :line |
		           line
			           regex: '-?\d+'
			           matchesCollect: [ :each | each asInteger ] ].

	^ AoCComputer
		  withProgram: numbers last
		  andRegA: numbers first first
		  andRegB: numbers second first
		  andRegC: numbers third first
]

{ #category : 'executing' }
AoCDay17 >> part1: lines [

	| computer |
	computer := self createComputer: lines.
	
	computer run.
	
	^computer output.
]
