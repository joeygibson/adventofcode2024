|lines fileRef list0 list1 splitLines diffs result|

fileRef := FileLocator home / 'Projects' / 'adventofcode2024' / 'day01' / 'input1.txt'.
lines := fileRef contents lines.

list0 := OrderedCollection new.
list1 := OrderedCollection new.

splitLines := lines collect: [ :line | line substrings ].
splitLines do: [ :line |
	list0 add: (line at: 1) asInteger.
	list1 add: (line at: 2) asInteger ].

list0 sort.
list1 sort.

diffs := list0 with: list1 collect: [ :i0 :i1 |
	 (i0 - i1) abs].
result := diffs sum.

"part 2"
(list0 collect: [ :num |
	num * (list1 occurrencesOf: num) ]) sum.
