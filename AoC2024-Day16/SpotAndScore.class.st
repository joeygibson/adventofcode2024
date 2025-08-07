Class {
	#name : 'SpotAndScore',
	#superclass : 'Object',
	#instVars : [
		'spot',
		'score'
	],
	#category : 'AoC2024-Day16',
	#package : 'AoC2024-Day16'
}

{ #category : 'as yet unclassified' }
SpotAndScore class >> withSpot: aPoint andScore: anInt [

	^ self new
		  spot: aPoint;
		  score: anInt
]

{ #category : 'accessing' }
SpotAndScore >> score [

	^ score
]

{ #category : 'accessing' }
SpotAndScore >> score: anObject [

	score := anObject
]

{ #category : 'accessing' }
SpotAndScore >> spot [

	^ spot
]

{ #category : 'accessing' }
SpotAndScore >> spot: anObject [

	spot := anObject
]
