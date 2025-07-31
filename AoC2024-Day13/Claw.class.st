"
The Claw Machine.
"
Class {
	#name : 'Claw',
	#superclass : 'Object',
	#instVars : [
		'a',
		'b',
		'prize'
	],
	#category : 'AoC2024-Day13',
	#package : 'AoC2024-Day13'
}

{ #category : 'as yet unclassified' }
Claw class >> withA: aPoint andB: bPoint andPrize: prizePoint [

	^ self new
		  a: aPoint;
		  b: bPoint;
		  prize: prizePoint
]

{ #category : 'accessing' }
Claw >> a [

	^ a
]

{ #category : 'accessing' }
Claw >> a: anObject [

	a := anObject
]

{ #category : 'accessing' }
Claw >> b [

	^ b
]

{ #category : 'accessing' }
Claw >> b: anObject [

	b := anObject
]

{ #category : 'as yet unclassified' }
Claw >> cramer [

	| wa wb |
	wa := prize x * b y - (prize y * b x) // (a x * b y - (a y * b x)).
	wb := a x * prize y - (a y * prize x) // (a x * b y - (a y * b x)).

	^ (wa * a x + (wb * b x) = prize x and:
		   wa * a y + (wb * b y) = prize y)
		  ifTrue: [ wa * 3 + wb ]
		  ifFalse: [ 0 ]
]

{ #category : 'accessing' }
Claw >> prize [

	^ prize
]

{ #category : 'accessing' }
Claw >> prize: anObject [

	prize := anObject
]
