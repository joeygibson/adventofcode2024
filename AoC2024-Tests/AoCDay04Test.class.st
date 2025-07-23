Class {
	#name : 'AoCDay04Test',
	#superclass : 'AoCBaseTest',
	#category : 'AoC2024-Tests',
	#package : 'AoC2024-Tests'
}

{ #category : 'initialization' }
AoCDay04Test >> createPuzzle: fileName [

	| lines puzzle |
	lines := self
		         readInputFileAsLines: fileName
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
	puzzle := self createPuzzle: 'input0.txt'.

	self assert: puzzle height equals: 10.
	self assert: puzzle width equals: 10.

	self assert: (puzzle atColumn: 5 atRow: 1) equals: $X
]

{ #category : 'tests' }
AoCDay04Test >> testFindAllInstancesOfLetter [

	| puzzle starts |
	puzzle := self createPuzzle: 'input0.txt'.

	starts := inst findAllInstances: puzzle of: $X.

	self assert: starts size equals: 19.

	starts := inst findAllInstances: puzzle of: $A.

	self assert: starts size equals: 24.
]

{ #category : 'tests' }
AoCDay04Test >> testGetNeighbors [

	| neighbors |
	neighbors := inst getNeighbors: 2 @ 2.

	self assert: neighbors isNotEmpty
]

{ #category : 'tests' }
AoCDay04Test >> testPart1WithRealData [

	| puzzle res |
	puzzle := self createPuzzle: 'input1.txt'.

	res := inst part1: puzzle.

	self assert: res equals: 2547.
]

{ #category : 'tests' }
AoCDay04Test >> testPart1WithSample [

	| res puzzle |
	puzzle := self createPuzzle: 'input0.txt'.

	res := inst part1: puzzle.

	self assert: res equals: 18
]

{ #category : 'tests' }
AoCDay04Test >> testPart2WithRealData [

	| res puzzle |
	puzzle := self createPuzzle: 'input1.txt'.

	res := inst part2: puzzle.

	self assert: res equals: 1939
]

{ #category : 'tests' }
AoCDay04Test >> testPart2WithSample [

	| res puzzle |
	puzzle := self createPuzzle: 'input0.txt'.

	res := inst part2: puzzle.

	self assert: res equals: 9
]
