"
Day09 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay09',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'executing' }
AoCDay09 >> compact2: map [

	| reversedMap workingMap mapSize fieldChunks |
	workingMap := map deepCopy.
	reversedMap := map reversed.
	mapSize := map size.
	fieldChunks := self
		               extractChunks: map
		               matching: [ :chr | chr ~= '.' ].

	fieldChunks reverse do: [ :fieldChunk |
			| len emptyChunk emptyChunks |
			emptyChunks := self
				               extractChunks: workingMap
				               matching: [ :chr | chr = '.' ].

			len := fieldChunk first size.

			emptyChunk := emptyChunks
				              detect: [ :chunk |
				              chunk first size >= len and:
					              chunk last < fieldChunk last ]
				              ifNone: nil.

			emptyChunk ifNotNil: [
				workingMap
					rotateFrom: fieldChunk last
					length: len
					to: emptyChunk last ] ].

	^ workingMap
]

{ #category : 'executing' }
AoCDay09 >> compact: map [

	| reversedMap workingMap mapSize |
	workingMap := map deepCopy.
	reversedMap := map reversed.
	mapSize := map size.

	[ self isCompact: workingMap ] whileFalse: [ "Transcript
				show: workingMap;
				cr."
			reversedMap doWithIndex: [ :chunk :chunkIndex |
					chunk ~= '.' ifTrue: [
							| emptyIndex reverseIndex |
							emptyIndex := workingMap indexOf: '.'.
							reverseIndex := mapSize - chunkIndex + 1.

							emptyIndex < reverseIndex ifTrue: [
								workingMap swapIndex: reverseIndex withIndex: emptyIndex ] ] ] ].

	^ workingMap
]

{ #category : 'executing' }
AoCDay09 >> createCompactMap: diskMap [

	| compactMap isFileIdentifier fileId |
	compactMap := OrderedCollection new.
	isFileIdentifier := true.
	fileId := 0.

	diskMap do: [ :diskLength |
			1 to: diskLength asString asInteger do: [ :l |
					isFileIdentifier
						ifTrue: [ compactMap add: fileId asString ]
						ifFalse: [ compactMap add: '.' ] ].

			isFileIdentifier ifTrue: [ fileId := fileId + 1 ].
			isFileIdentifier := isFileIdentifier not ].

	^ compactMap
]

{ #category : 'executing' }
AoCDay09 >> extractChunks: map matching: aBlock [

	| block lastChr chunks startIndex inChunk |
	chunks := OrderedCollection new.
	block := OrderedCollection new.
	lastChr := nil.
	startIndex := 1.
	inChunk := false.

	1 to: map size do: [ :i |
			| chr |
			chr := map at: i.

			(aBlock value: chr)
				ifTrue: [
						inChunk ifFalse: [
								startIndex := i.
								inChunk := true ].

						(chr ~= lastChr and: block isNotEmpty) ifTrue: [
								chunks add: {
										block.
										startIndex }.
								block := OrderedCollection new.
								startIndex := i ].
						block add: chr.
						lastChr := chr ]
				ifFalse: [
						inChunk := false.
						block isNotEmpty ifTrue: [
								chunks add: {
										block.
										startIndex }.
								block := OrderedCollection new ].
						lastChr := chr ] ].

	block ifNotEmpty: [
			chunks add: {
					block.
					startIndex } ].

	^ chunks
]

{ #category : 'testing' }
AoCDay09 >> isCompact: map [

	| dotIndex head tail reversedMap |
	dotIndex := map indexOf: '.'.
	head := map takeWhile: [ :item | item ~= '.' ].

	reversedMap := map reversed.
	dotIndex := reversedMap indexOf: '.'.
	tail := reversedMap takeWhile: [ :item | item = '.' ].

	^ head size + tail size = map size
]

{ #category : 'executing' }
AoCDay09 >> part1: lines [

	| diskMap compactMap compactedMap fileIds checkSum |
	diskMap := lines at: 1.
	compactMap := self createCompactMap: diskMap.

	compactedMap := self compact: compactMap.
	fileIds := compactedMap takeWhile: [ :item | item ~= '.' ].
	checkSum := fileIds collectWithIndex: [ :item :index |
		            item asInteger * (index - 1) ].

	^ checkSum sum
]

{ #category : 'executing' }
AoCDay09 >> part2: lines [

	| diskMap compactMap compactedMap checkSum |
	diskMap := lines first.
	compactMap := self createCompactMap: diskMap.

	compactedMap := self compact2: compactMap.

	checkSum := compactedMap collectWithIndex: [ :item :index |
			            item = '.'
				            ifTrue: [ 0 ]
				            ifFalse: [ item asInteger * (index - 1) ] ].

	^ checkSum sum
]
