Class {
	#name : 'FloorTile',
	#superclass : 'Object',
	#instVars : [
		'visited',
		'visitCount'
	],
	#category : 'AoC2024-Day06',
	#package : 'AoC2024-Day06'
}

{ #category : 'adding' }
FloorTile >> addVisit [
	visitCount := visitCount +1
]

{ #category : 'initialization' }
FloorTile >> initialize [

	visited := false.
	visitCount := 0
]

{ #category : 'accessing' }
FloorTile >> visitCount [

	^ visitCount
]

{ #category : 'accessing' }
FloorTile >> visitCount: anObject [

	visitCount := anObject
]

{ #category : 'accessing' }
FloorTile >> visited [

	^ visited
]

{ #category : 'accessing' }
FloorTile >> visited: anObject [

	visited := anObject
]
