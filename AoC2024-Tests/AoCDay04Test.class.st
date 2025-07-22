Class {
	#name : 'AoCDay04Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'initialization' }
AoCDay04Test >> createPuzzle [

	| lines puzzle |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	lines := lines reject: [ :line | line isEmpty ].

	puzzle := CTArray2D width: lines first size height: lines size.

	lines doWithIndex: [ :line :row |
			line asArray doWithIndex: [ :c :col |
				puzzle atColumn: col atRow: row put: c ] ].

	^ puzzle
]

{ #category : 'running' }
AoCDay04Test >> setUp [
	super setUp.
	self day: '04'.
	self path: FileLocator home / 'Projects' / 'adventofcode2024' asString.
	
	inst := AoCDay04 new.
]

{ #category : 'tests' }
AoCDay04Test >> testCreatePuzzle [

	| puzzle |
	puzzle := self createPuzzle.

	self assert: puzzle height equals: 10.
	self assert: puzzle width equals: 10.

	self assert: (puzzle atColumn: 5 atRow: 1) equals: $X
]

{ #category : 'tests' }
AoCDay04Test >> testFindAllStarts [

	| puzzle starts |
	puzzle := self createPuzzle.

	starts := inst findAllStarts: puzzle.

	self assert: (starts size) equals: 19
]

{ #category : 'tests' }
AoCDay04Test >> testGetNeighbors [

	| neighbors |
	neighbors := inst getNeighbors: 2 @ 2.

	Transcript
		show: neighbors;
		cr.

	self assert: neighbors isNotEmpty
]

{ #category : 'tests' }
AoCDay04Test >> testPart1WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part1: lines.

	self assert: res equals: 161.
]

{ #category : 'tests' }
AoCDay04Test >> testPart1WithSample [

	| res puzzle |
	puzzle := self createPuzzle.

	res := inst part1: puzzle.

	self assert: res equals: 18
]

{ #category : 'tests' }
AoCDay04Test >> testPart2WithRealData [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input1.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 161.
]

{ #category : 'tests' }
AoCDay04Test >> testPart2WithSample [

	| lines res |
	lines := self
		         readInputFileAsLines: 'input0.txt'
		         for: self day
		         onPath: self path.

	res := inst part2: lines.

	self assert: res equals: 161.
]
