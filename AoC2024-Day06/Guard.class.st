Class {
	#name : 'Guard',
	#superclass : 'Object',
	#instVars : [
		'direction',
		'col',
		'row'
	],
	#category : 'AoC2024-Day06',
	#package : 'AoC2024-Day06'
}

{ #category : 'accessing' }
Guard >> col [

	^ col
]

{ #category : 'accessing' }
Guard >> col: anObject [

	col := anObject
]

{ #category : 'accessing' }
Guard >> direction [

	^ direction
]

{ #category : 'accessing' }
Guard >> direction: anObject [

	direction := anObject
]

{ #category : 'initialization' }
Guard >> initialize [
	direction := #up.
	col := 0.
	row := 0.
]

{ #category : 'printing' }
Guard >> printOn: aStream [

	aStream
		nextPutAll: 'Guard(';
		nextPutAll: 'dir: ';
		nextPutAll: self direction asString;
		nextPutAll: ', col: ';
		nextPutAll: self col asString;
		nextPutAll: ', row: ';
		nextPutAll: self row asString;
		nextPutAll: ')'
]

{ #category : 'accessing' }
Guard >> row [

	^ row
]

{ #category : 'accessing' }
Guard >> row: anObject [

	row := anObject
]
