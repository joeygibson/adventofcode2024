"
Day23 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay23',
	#superclass : 'AoCBase',
	#instVars : [
		'network'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay23 >> createNetwork: lines [

	network := Dictionary new.

	lines do: [ :line |
			| comps a b |
			comps := line substrings: '-'.
			a := comps first.
			b := comps second.

			network
				at: a
				update: [ :v |
						v
							add: b;
							yourself ]
				initial: (Set with: b).

			network
				at: b
				update: [ :v |
						v
							add: a;
							yourself ]
				initial: (Set with: a) ]
]

{ #category : 'executing' }
AoCDay23 >> part1: lines [

	| triples |
	self createNetwork: lines.

	triples := Set new.
	(network keys select: [ :t | t beginsWith: 't' ]) collect: [ :t |
			(network at: t) do: [ :a |
					(network at: a) do: [ :b |
							((network at: t) includes: b) ifTrue: [
									triples add: {
											t.
											a.
											b } sorted ] ] ] ].

	^ triples size
]
