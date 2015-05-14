""
PROJECTNAME debugTypeWise .
PROJECTCATEGORY  .

CLASS TypeWiseDebugStateHolder
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asString
END
! !
Object subclass: #TypeWiseDebugStateHolder
	instanceVariableNames: ''
	classVariableNames: 'symbolAsStringCalled '
	poolDictionaries: ''
	comment: ''
	category: ''!

!TypeWiseDebugStateHolder class * methods 08:47 - 05/24/04!
reset
	self symbolAsStringCalled: false.! !

!TypeWiseDebugStateHolder class * methods 08:46 - 05/24/04!
symbolAsStringCalled
	symbolAsStringCalled isNil ifTrue: [ symbolAsStringCalled := false ].
	^symbolAsStringCalled! !

!TypeWiseDebugStateHolder class * methods 08:47 - 05/24/04!
symbolAsStringCalled: aBoolState
	symbolAsStringCalled := aBoolState! !

!Symbol .INTERNALDEV methods 08:47 - 05/24/04!
asString
	"
	Answers the string that corresponds to the symbol.
	Return Value:
		A StringA.
	"
	TypeWiseDebugStateHolder symbolAsStringCalled ifFalse: [ TypeWiseDebugStateHolder symbolAsStringCalled: true. self halt ].
	^(SymbolTable at: self)! !

Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: '.INTERNALDEV'A text: '.INTERNALDEV
This category marks methods that are not included in runtime images.'A.!
