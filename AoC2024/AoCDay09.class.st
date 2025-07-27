"
Day09 of Advent of Code 2024.
"
Class {
	#name : 'AoCDay09',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'as yet unclassified' }
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

{ #category : 'as yet unclassified' }
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
