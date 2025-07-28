Class {
	#name : 'AoCDay09Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'running' }
AoCDay09Test >> setUp [
	super setUp.
	self day: '09'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay09 new.
]

{ #category : 'tests' }
AoCDay09Test >> testExtractChunksMatching [

	| map fileIdChunks emptyChunks |
	map := '00...111...2...333.44.5555.6666.777.888899'
		       asOrderedCollection collect: [ :c | c asString ].

	fileIdChunks := inst
		                extractChunks: map
		                matching: [ :chr | chr ~= '.' ].

	self assert: fileIdChunks size equals: 10.

	self
		assert: fileIdChunks first first
		equals: #( '0' '0' ) asOrderedCollection.
	self assert: fileIdChunks first last equals: 1.
	self
		assert: fileIdChunks last first
		equals: #( '9' '9' ) asOrderedCollection.
	self assert: fileIdChunks last last equals: 41.

	emptyChunks := inst extractChunks: map matching: [ :chr | chr = '.' ].

	self assert: emptyChunks size equals: 8.

	self
		assert: emptyChunks first first
		equals: #( '.' '.' '.' ) asOrderedCollection.
	self assert: emptyChunks first last equals: 3.
	self
		assert: emptyChunks last first
		equals: #( '.' ) asOrderedCollection.
	self assert: emptyChunks last last equals: 36
]

{ #category : 'tests' }
AoCDay09Test >> testIsCompact [

	| s |
	s := OrderedCollection withAll: #('1' '2' '3' '.' '.' '.').
	self assert: (inst isCompact: s).

	s := OrderedCollection withAll: #('1' '2' '3' '.' '.' '.' '4' '5' '6').
	self deny: (inst isCompact: s)
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithOtherSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input2.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 60
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self timeLimit: (Duration seconds: 600).

	res := inst part1: lines.

	self assert: res equals: 6353658451014
]

{ #category : 'tests' }
AoCDay09Test >> testPart1WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 1928
]

{ #category : 'tests' }
AoCDay09Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	self timeLimit: (Duration seconds: 600).

	res := inst part2: lines.

	self assert: res equals: 6382582136592
]

{ #category : 'tests' }
AoCDay09Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 2858
]

{ #category : 'tests' }
AoCDay09Test >> testRotate [

	| map fileChunk emptyChunk |
	map := '00...111...2...333.44.5555.6666.777.888899'
		       asOrderedCollection collect: [ :c | c asString ].

	fileChunk := {
		             #( '9' '9' ).
		             41 }.
	emptyChunk := {
		              #( '.' '.' '.' ).
		              3 }.

	map rotateFrom: 41 length: 2 to: 3.

	self
		assert: (map copyFrom: 1 to: 5) asArray
		equals: #( '0' '0' '9' '9' '.' )
]
