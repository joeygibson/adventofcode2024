"
Please describe the package using the class comment of the included manifest class. The manifest class also includes other additional metadata for the package. These meta data are used by other tools such as the SmalllintManifestChecker and the critics Browser
"
Class {
	#name : 'ManifestAoC2024',
	#superclass : 'PackageManifest',
	#category : 'AoC2024-Manifest',
	#package : 'AoC2024',
	#tag : 'Manifest'
}

{ #category : 'code-critics' }
ManifestAoC2024 class >> ruleLongMethodsRuleV1FalsePositive [

	<ignoreForCoverage>
	^ #(#(#(#RGPackageDefinition #(#AoC2024)) #'2025-07-30T11:56:02.619175-04:00') )
]
