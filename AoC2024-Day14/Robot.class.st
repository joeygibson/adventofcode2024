"
It's a Robot!

"
Class {
	#name : 'Robot',
	#superclass : 'Object',
	#instVars : [
		'pos',
		'dx',
		'dy',
		'roomHeight',
		'roomWidth'
	],
	#category : 'AoC2024-Day14',
	#package : 'AoC2024-Day14'
}

{ #category : 'accessing' }
Robot class >> startingAt: aPoint dx: dx dy: dy [

	^ self new
		  pos: aPoint;
		  dx: dx;
		  dy: dy
]

{ #category : 'accessing' }
Robot class >> startingAt: aPoint dx: dx dy: dy roomHeight: height roomWidth: width [

	^ self new
		  pos: aPoint;
		  dx: dx;
		  dy: dy;
		  roomHeight: height;
		  roomWidth: width
]

{ #category : 'accessing' }
Robot >> dx [

	^ dx
]

{ #category : 'accessing' }
Robot >> dx: anObject [

	dx := anObject
]

{ #category : 'accessing' }
Robot >> dy [

	^ dy
]

{ #category : 'accessing' }
Robot >> dy: anObject [

	dy := anObject
]

{ #category : 'accessing' }
Robot >> move [

	| newX newY |
	newX := pos x + dx \\ roomWidth.
	newY := pos y + dy \\ roomHeight.

	self pos: newX @ newY.

	^ self
]

{ #category : 'accessing' }
Robot >> pos [

	^ pos
]

{ #category : 'accessing' }
Robot >> pos: anObject [

	pos := anObject
]

{ #category : 'printing' }
Robot >> printOn: aStream [

	aStream
		nextPutAll: 'aRobot: ';
		nextPutAll: pos asString;
		nextPutAll: ', dx: ';
		nextPutAll: dx asString;
		nextPutAll: ', dy; ';
		nextPutAll: dy asString;
		cr
]

{ #category : 'accessing' }
Robot >> roomHeight [

	^ roomHeight
]

{ #category : 'accessing' }
Robot >> roomHeight: anObject [

	roomHeight := anObject
]

{ #category : 'accessing' }
Robot >> roomWidth [

	^ roomWidth
]

{ #category : 'accessing' }
Robot >> roomWidth: anObject [

	roomWidth := anObject
]
