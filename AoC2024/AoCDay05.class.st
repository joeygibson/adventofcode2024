Class {
	#name : 'AoCDay05',
	#superclass : 'AoCBase',
	#category : 'AoC2024',
	#package : 'AoC2024'
}

{ #category : 'testing' }
AoCDay05 >> isValidPage: page fromUpdate: update withRules: ruleMap [

	| previousPages includedPages pagePosition |
	previousPages := ruleMap at: page ifAbsent: #().
	includedPages := previousPages intersection: update.
	pagePosition := update detectIndex: [ :item | item = page ].

	^ includedPages allSatisfy: [ :previousPage |
			  | previousPagePosition |
			  previousPagePosition := update detectIndex: [ :item |
				                          item = previousPage ].
			  previousPagePosition < pagePosition ]
]

{ #category : 'executing' }
AoCDay05 >> makeRuleTable: rules [

	| ruleMap |
	ruleMap := Dictionary new.

	rules do: [ :line |
			| previousPage page |
			previousPage := line first.
			page := line second.
	"		Transcript show: ('{1}, {2}, {3} -> {4}' 
			format: { line. previousPage. page . ruleMap at: page ifAbsent: 'nil'}); cr."
			(ruleMap at: page ifAbsentPut: [ OrderedCollection new ]) addFirst:
				previousPage ].
"	Transcript show: '----'; cr.
	Transcript show: (ruleMap at: 75 ifAbsent: 'fuck'); cr."
	^ ruleMap
]

{ #category : 'parsing' }
AoCDay05 >> parse: lines [

	| rawRules rawUpdates emptyIndex rules updates |
	emptyIndex := lines detectIndex: [ :line | line isEmpty ].

	rawRules := lines takeFirst: emptyIndex - 1.
	rawUpdates := lines allButFirst: emptyIndex.

	rules := rawRules collect: [ :line |
			         | chunks |
			         chunks := (line substrings: '|') collect: [ :chunk |
				                   chunk asInteger ] ].
	updates := rawUpdates collect: [ :line |
			           | chunks |
			           chunks := (line substrings: ',') collect: [ :chunk |
				                     chunk asInteger ] ].

	^ {
		  rules.
		  updates }
]

{ #category : 'executing' }
AoCDay05 >> part1: lines [

	| data rules updates ruleMap validUpdates middlePages |
	data := self parse: lines.
	rules := data first.
	updates := data second.

	ruleMap := self makeRuleTable: rules.
	validUpdates := self validateUpdates: updates withRules: ruleMap.

	middlePages := validUpdates collect: [ :update |
		               update at: update size // 2 + 1 ].

	^ middlePages sum
]

{ #category : 'as yet unclassified' }
AoCDay05 >> validateUpdates: updates withRules: ruleMap [

	^ updates select: [ :update |
			  update allSatisfy: [ :page |
				  self isValidPage: page fromUpdate: update withRules: ruleMap ] ]
]
