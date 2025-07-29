"
Day11 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay11',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay11 >> blink: stones times: times [

	| work |
	work := stones copy.

	times timesRepeat: [
			work := work flatCollect: [ :stone |
					        stone = 0
						        ifTrue: [ { 1 } ]
						        ifFalse: [
								        | stoneStr |
								        stoneStr := stone asString.

								        stoneStr size even
									        ifTrue: [
											        | left right mid |
											        mid := stoneStr size // 2.

											        left := (stoneStr allButLast: mid) asInteger.
											        right := (stoneStr allButFirst: mid) asInteger.

											        {
												        left.
												        right } ]
									        ifFalse: [ { (stone * 2024) } ] ] ] ].

	^ work
]

{ #category : 'executing' }
AoCDay11 >> part1: lines [

	| stones |
	stones := lines first substrings asArray collect: [ :stone |
		          stone asString asInteger ].

	stones := self blink: stones times: 25.

	^ stones size
]
