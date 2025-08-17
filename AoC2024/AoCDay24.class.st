"
Day24 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay24',
	#superclass : 'AoCBase',
	#instVars : [
		'wires',
		'gates'
	],
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
AoCDay24 >> getOutputValue [

	| zWireNames sortedNames zWireValues res bString |
	zWireNames := wires keys select: [ :name | name beginsWith: 'z' ].

	sortedNames := zWireNames sorted reverse.
	zWireValues := sortedNames collect: [ :name |
		               (wires at: name) value asString ].
	bString := zWireValues joinUsing: ''.
	res := Integer readFrom: bString readStream base: 2.

	^ res
]

{ #category : 'as yet unclassified' }
AoCDay24 >> parse: lines [

	| sections |
	sections := lines splitOn: [ :line | line isEmpty ].

	wires := (sections first collect: [ :line |
			          | chunks |
			          chunks := line substrings: ':'.
			          chunks first
			          ->
			          (Wire
				           named: chunks first
				           value: chunks second asString asInteger) ])
		         asDictionary.

	gates := sections second collect: [ :line |
			         | chunks input1Name input2Name outputName gateName input1 input2 output |
			         chunks := line substrings.
			         input1Name := chunks first.
			         gateName := chunks second.
			         input2Name := chunks third.
			         outputName := chunks fifth.

			         input1 := wires
				                   at: input1Name
				                   ifAbsentPut: [
				                   Wire named: input1Name value: nil ].

			         input2 := wires
				                   at: input2Name
				                   ifAbsentPut: [
				                   Wire named: input2Name value: nil ].

			         output := wires
				                   at: outputName
				                   ifAbsentPut: [
				                   Wire named: outputName value: nil ].

			         Gate
				         type: gateName
				         input1: input1
				         input2: input2
				         output: output ]
]

{ #category : 'executing' }
AoCDay24 >> part1: lines [

	self parse: lines.

	[ gates allSatisfy: [ :gate | gate tripped ] ] whileFalse: [
		gates do: [ :gate | gate tripped ifFalse: [ gate trip ] ] ].

	^ self getOutputValue
]
