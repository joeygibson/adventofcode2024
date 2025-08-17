Class {
	#name : 'AndGate',
	#superclass : 'Gate',
	#category : 'AoC2024-Day24',
	#package : 'AoC2024-Day24'
}

{ #category : 'as yet unclassified' }
AndGate >> trip [

	(input1 isEnergized and: input2 isEnergized) ifFalse: [ ^ self ].
	self tripped: true.

	(input1 isOn and: input2 isOn)
		ifTrue: [ output turnOn ]
		ifFalse: [ output turnOff ]
]
