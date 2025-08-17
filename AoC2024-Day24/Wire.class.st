Class {
	#name : 'Wire',
	#superclass : 'Object',
	#instVars : [
		'name',
		'value'
	],
	#category : 'AoC2024-Day24',
	#package : 'AoC2024-Day24'
}

{ #category : 'as yet unclassified' }
Wire class >> named: name value: aValue [

	^ self new
		  name: name;
		  value: aValue
]

{ #category : 'testing' }
Wire >> isEnergized [
	^value isNotNil 
]

{ #category : 'testing' }
Wire >> isOff [

	^ value = 0
]

{ #category : 'testing' }
Wire >> isOn [

	^ value = 1
]

{ #category : 'accessing' }
Wire >> name [

	^ name
]

{ #category : 'accessing' }
Wire >> name: anObject [

	name := anObject
]

{ #category : 'printing' }
Wire >> printOn: aStream [

	aStream
		nextPutAll: 'Wire(';
		nextPutAll: name;
		nextPutAll: ', ';
		nextPutAll: value asString;
		nextPutAll: ')'
]

{ #category : 'testing' }
Wire >> turnOff [

	value := 0
]

{ #category : 'testing' }
Wire >> turnOn [

	value := 1
]

{ #category : 'accessing' }
Wire >> value [

	^ value
]

{ #category : 'accessing' }
Wire >> value: anObject [

	value := anObject
]
