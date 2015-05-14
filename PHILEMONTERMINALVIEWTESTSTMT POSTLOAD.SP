""
PROJECTNAME "PhilemonTerminalViewTestStMt Postload" .
PROJECTCATEGORY "Philemon Terminal View" .

CLASS TerminalTests
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	testList
END
! !

!TerminalTests publicMethods methods 11:23 - 03/12/04!
testList
	| list |
	list := TerminalList new.
	list items: #(1 2 3 4).
	list selectionIndex: 0.
	self assert: list selectedItem isNil.
	self assert: list selectionIndex = 0.
	self assert: list items size = 4.
	list selectionIndex: 2.
	self assert: list selectedItem = 2.
	"self should: [list selectionIndex: nil] raise: ExError.
	self should: [list selectionIndex: 5] raise: ExError."! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
