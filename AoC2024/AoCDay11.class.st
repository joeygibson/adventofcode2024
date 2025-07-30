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
AoCDay11 >> blink2: rock [

	| rockAsNum rockLen res |
	rockAsNum := rock asString asInteger.
	rockLen := rock size.

	rockAsNum = 0
		ifTrue: [ res := { '1' } ]
		ifFalse: [
				rockLen even
					ifTrue: [
							| mid left right |
							mid := rockLen // 2.
							left := rock allButLast: mid.
							right := (rock allButFirst: mid) asInteger asString.
							res := {
								       left.
								       right } ]
					ifFalse: [ res := { (rockAsNum * 2048) asString } ] ].

	^ res
]

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

{ #category : 'executing' }
AoCDay11 >> part2: lines [

	| stones |
	stones := Dictionary new.

	lines first substrings do: [ :stone |
		stones at: stone update: [ :v | v + 1 ] initial: 1 ].

	75 timesRepeat: [
			| newStones |
			newStones := Dictionary new.

			stones keysAndValuesDo: [ :stone :count |
					(self blink2: stone) do: [ :newStone |
						newStones at: newStone update: [ :v | v + count ] initial: 1 ] ].
			stones := newStones copy ].

	^ stones values sum
]
