"
Computer for AoC2024 Day 17.
"
Class {
	#name : 'AoCComputer',
	#superclass : 'Object',
	#instVars : [
		'regA',
		'regB',
		'regC',
		'intPointer',
		'buffer',
		'program',
		'jumped'
	],
	#category : 'AoC2024-Day17',
	#package : 'AoC2024-Day17'
}

{ #category : 'as yet unclassified' }
AoCComputer class >> withProgram: program andRegA: regA andRegB: regB andRegC: regC [

	^ self new
		  program: program;
		  regA: regA;
		  regB: regB;
		  regC: regC
]

{ #category : 'as yet unclassified' }
AoCComputer >> adv: operand [

	| num den |
	num := self regA.
	den := 2 ** (self handleOperand: operand).

	self regA: num // den
]

{ #category : 'as yet unclassified' }
AoCComputer >> bdv: operand [

	| num den |
	num := self regA.
	den := 2 ** (self handleOperand: operand).

	self regB: num // den
]

{ #category : 'as yet unclassified' }
AoCComputer >> bst: operand [

	| op |
	op := self handleOperand: operand.
	self regB: op \\ 8
]

{ #category : 'accessing' }
AoCComputer >> buffer [

	^ buffer
]

{ #category : 'accessing' }
AoCComputer >> buffer: anObject [

	buffer := anObject
]

{ #category : 'as yet unclassified' }
AoCComputer >> bxc: operand [

	| valB valC |
	valB := self regB.
	valC := self regC.

	self regB: (valB bitXor: valC).
]

{ #category : 'as yet unclassified' }
AoCComputer >> bxl: operand [

	| reg |
	reg := self regB.

	self regB: (reg bitXor: operand)
]

{ #category : 'as yet unclassified' }
AoCComputer >> cdv: operand [

	| num den |
	num := self regA.
	den := 2 ** (self handleOperand: operand).

	self regC: num // den
]

{ #category : 'as yet unclassified' }
AoCComputer >> handleOperand: operand [

	(operand between: 0 and: 3) ifTrue: [ ^ operand ].
	operand = 4 ifTrue: [ ^ self regA ].
	operand = 5 ifTrue: [ ^ self regB ].
	operand = 6 ifTrue: [ ^ self regC ].
	operand = 7 ifTrue: [ ^ 0 ]
]

{ #category : 'initialization' }
AoCComputer >> initialize [

	self
		intPointer: 1;
		jumped: false;
		buffer: OrderedCollection new
]

{ #category : 'accessing' }
AoCComputer >> intPointer [

	^ intPointer
]

{ #category : 'accessing' }
AoCComputer >> intPointer: anObject [

	intPointer := anObject
]

{ #category : 'as yet unclassified' }
AoCComputer >> jnz: operand [

	| val |
	val := self regA.

	val ~= 0 ifTrue: [ self intPointer: operand + 1. self jumped: true ]
]

{ #category : 'accessing' }
AoCComputer >> jumped [

	^ jumped
]

{ #category : 'accessing' }
AoCComputer >> jumped: anObject [

	jumped := anObject
]

{ #category : 'as yet unclassified' }
AoCComputer >> out: operand [

	| op |
	op := self handleOperand: operand.

	buffer add: op \\ 8
]

{ #category : 'accessing' }
AoCComputer >> output [

	^ buffer joinUsing: ','
]

{ #category : 'as yet unclassified' }
AoCComputer >> processOpcode: opCode withOperand: operand [

	| ptrInc |
	opCode
		caseOf: {
				([ 0 ] -> [ self adv: operand ]).
				([ 1 ] -> [ self bxl: operand ]).
				([ 2 ] -> [ self bst: operand ]).
				([ 3 ] -> [ self jnz: operand ]).
				([ 4 ] -> [ self bxc: operand ]).
				([ 5 ] -> [ self out: operand ]).
				([ 6 ] -> [ self bdv: operand ]).
				([ 7 ] -> [ self cdv: operand ]) }
		otherwise: [ self error: 'Invalid instruction' ]
]

{ #category : 'accessing' }
AoCComputer >> program [

	^ program
]

{ #category : 'accessing' }
AoCComputer >> program: anObject [

	program := anObject
]

{ #category : 'accessing' }
AoCComputer >> regA [

	^ regA
]

{ #category : 'accessing' }
AoCComputer >> regA: anObject [

	regA := anObject
]

{ #category : 'accessing' }
AoCComputer >> regB [

	^ regB
]

{ #category : 'accessing' }
AoCComputer >> regB: anObject [

	regB := anObject
]

{ #category : 'accessing' }
AoCComputer >> regC [

	^ regC
]

{ #category : 'accessing' }
AoCComputer >> regC: anObject [

	regC := anObject
]

{ #category : 'running' }
AoCComputer >> run [

	[ intPointer < program size ] whileTrue: [
			| opCode operand |
			opCode := program at: intPointer.
			operand := program at: intPointer + 1.

			self processOpcode: opCode withOperand: operand.

			jumped ifFalse: [ intPointer := intPointer + 2 ].

			jumped := false ]
]
