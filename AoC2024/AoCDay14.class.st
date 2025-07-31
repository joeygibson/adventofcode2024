"
Day14 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay14',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'parsing' }
AoCDay14 >> parse: lines roomWidth: width roomHeight: height [

	^ lines collect: [ :line |
			  | numbers pos dx dy |
			  numbers := line
				             regex: '-?\d+'
				             matchesCollect: [ :each | each asInteger ].
			  pos := numbers first @ numbers second.
			  dx := numbers third.
			  dy := numbers fourth.

			  Robot
				  startingAt: pos
				  dx: dx
				  dy: dy
				  roomHeight: height
				  roomWidth: width ]
]

{ #category : 'executing' }
AoCDay14 >> part1: lines which: which [

	| robots quads roomHeight roomWidth vRoomDiv hRoomDiv |
	roomWidth := which = #sample
		             ifTrue: [ 11 ]
		             ifFalse: [ 101 ].
	roomHeight := which = #sample
		              ifTrue: [ 7 ]
		              ifFalse: [ 103 ].
	vRoomDiv := roomWidth // 2.
	hRoomDiv := roomHeight // 2.


	robots := self
		          parse: lines
		          roomWidth: roomWidth
		          roomHeight: roomHeight.
	quads := Dictionary new.

	0 to: 99 do: [ :i | robots do: [ :robot | robot move ] ].

	robots do: [ :robot |
			| xQuad yQuad |
			xQuad := robot pos x < vRoomDiv
				         ifTrue: [ 0 ]
				         ifFalse: [ robot pos x > vRoomDiv ifTrue: [ 1 ] ].
			yQuad := robot pos y < hRoomDiv
				         ifTrue: [ 0 ]
				         ifFalse: [ robot pos y > hRoomDiv ifTrue: [ 1 ] ].
			(xQuad isNotNil and: yQuad isNotNil) ifTrue: [
				quads at: xQuad @ yQuad update: [ :v | v + 1 ] initial: 1 ] ].

	^ quads inject: 1 into: [ :v :total | total * v ]
]
