"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestAoC2024Day17',
	#superclass : 'PackageManifest',
	#category : 'AoC2024-Day17-Manifest',
	#package : 'AoC2024-Day17',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestAoC2024Day17 class >> ruleUnclassifiedMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGMethodDefinition #(#'AoCComputer class' #withProgram:andRegA:andRegB:andRegC: #true)) #'2025-08-06T09:08:54.477501-04:00') )
]
