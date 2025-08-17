Class {
	#name : 'Gate',
	#superclass : 'Object',
	#instVars : [
		'input1',
		'input2',
		'output',
		'tripped'
	],
	#category : 'AoC2024-Day24',
	#package : 'AoC2024-Day24'
}

{ #category : 'as yet unclassified' }
Gate class >> input1: i1 input2: i2 output: o1 [

	^ self new
		  input1: i1;
		  input2: i2;
		  output: o1;
		  tripped: false
]

{ #category : 'as yet unclassified' }
Gate class >> type: whatType input1: i1 input2: i2 output: o1 [

	whatType = 'AND' ifTrue: [
		^ AndGate input1: i1 input2: i2 output: o1 ].
	whatType = 'OR' ifTrue: [ ^ OrGate input1: i1 input2: i2 output: o1 ].
	whatType = 'XOR' ifTrue: [
		^ XorGate input1: i1 input2: i2 output: o1 ]
]

{ #category : 'accessing' }
Gate >> input1 [

	^ input1
]

{ #category : 'accessing' }
Gate >> input1: anObject [

	input1 := anObject
]

{ #category : 'accessing' }
Gate >> input2 [

	^ input2
]

{ #category : 'accessing' }
Gate >> input2: anObject [

	input2 := anObject
]

{ #category : 'accessing' }
Gate >> output [

	^ output
]

{ #category : 'accessing' }
Gate >> output: anObject [

	output := anObject
]

{ #category : 'printing' }
Gate >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPutAll: '(';
		nextPutAll: input1 asString;
		nextPutAll: ', ';
		nextPutAll: input2 asString;
		nextPutAll: ', ';
		nextPutAll: output asString;
		nextPutAll: ', ';
		nextPutAll: tripped asString;
		nextPutAll: ')'
]

{ #category : 'as yet unclassified' }
Gate >> trip [

	self subclassResponsibility
]

{ #category : 'accessing' }
Gate >> tripped [

	^ tripped
]

{ #category : 'accessing' }
Gate >> tripped: anObject [

	tripped := anObject
]
