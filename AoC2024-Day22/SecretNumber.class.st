Class {
	#name : 'SecretNumber',
	#superclass : 'Object',
	#instVars : [
		'currentValue'
	],
	#category : 'AoC2024-Day22',
	#package : 'AoC2024-Day22'
}

{ #category : 'instance creation' }
SecretNumber class >> with: anInteger [

	^ self new currentValue: anInteger
]

{ #category : 'accessing' }
SecretNumber >> currentValue [

	^ currentValue
]

{ #category : 'accessing' }
SecretNumber >> currentValue: anObject [

	currentValue := anObject
]

{ #category : 'as yet unclassified' }
SecretNumber >> evolve [

	| workingValue |
	workingValue := currentValue * 64.
	workingValue := currentValue bitXor: workingValue.
	currentValue := workingValue \\ 16777216.

	workingValue := currentValue // 32.
	workingValue := currentValue bitXor: workingValue.
	currentValue := workingValue \\ 16777216.

	workingValue := currentValue * 2048.
	workingValue := currentValue bitXor: workingValue.
	currentValue := workingValue \\ 16777216.


	^ currentValue
]

{ #category : 'printing' }
SecretNumber >> printOn: aStream [

	aStream
		nextPutAll: 'SecretNumber(';
		nextPutAll: self currentValue asString;
		nextPutAll: ')'
]
