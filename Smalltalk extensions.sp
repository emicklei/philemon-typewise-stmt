""
PROJECTNAME "smalltalk extensions" .
PROJECTCATEGORY  .
PREREQUISITES sunit2 .


PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	currentSelector
	currentMethodSelector
END

CLASS Behavior
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	compiledMethodAt:
	sourceCodeAt:
END

CLASS Core::MethodDescriptor
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	referencesLiteral:
	allLiterals
END

CLASS Smalltalk
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	isVariableBinding:
	allBehaviorsDo:
END
INSTANCEMETHODS 
BEGIN
END

CLASS Class
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	classPool
END

CLASS Metaclass
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	soleInstance
END

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	isVariableBinding
END

CLASS SystemTest
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *
! !
TestCase subclass: #SystemTest
	instanceVariableNames: ''
	classVariableNames: 'ClassVar1 ClassVar2 '
	poolDictionaries: ''
	comment: ''
	category: ''!

!Object Accessing methods 07:07 - 03/16/00!
currentMethodSelector
	"
	Returns the selector of the current method (the one currently executing).
	Return Value: 
		A String.
	"
	|ret|
	ret := _asm{ mov eax, [ebp+4] }.
	(ret := Exception methodFromVA: ret basicAddress) notNil ifTrue: [
		^ret at: 2
	].
	^nil! !

!Object Accessing methods 06:44 - 03/16/00!
currentSelector
	"
	Returns the selector of the current method (the one currently executing).
	Return Value: 
		A String.
	"
	|ret|
	ret := _asm{ mov eax, [ebp+4] }.
	(ret := Exception methodFromVA: ret basicAddress) notNil ifTrue: [
		^ret at: 2
	].
	^nil! !

!Behavior .INTERNALDEV methods 05:44 - 03/17/00!
compiledMethodAt: aSymbol
	"
	Answers the method descriptor at a specified symbol.
	Parameters:
		aSymbol    A Symbol.
	Return Value: 
		A MethodDescriptor.
	"
	^self methodDictionary at: aSymbol asString! !

!Behavior .INTERNALDEV methods 12:39 - 03/15/00!
sourceCodeAt: aSymbol
	"
	Returns the source code at a specified symbol.
	Parameters: 
		aSymbol    A Symbol or String.
	Return Value: 
		A String if successful, otherwise the method raises an exception.
	"
	^(self methodDictionary at: aSymbol asString) source! !

!Core::MethodDescriptor .INTERNALDEV methods 06:50 - 03/17/00!
allLiterals
	"
	Answers all literals defined in the method.
	Return Value: 
		A collection of method literals.
	"
	|va answer sizeOfData ptr cb|
	(va := self smalltalkClass instanceMethodAddressExactAt: m_methodID
		flags: FALSE) == 0 ifTrue: [^EmptyArray].
	sizeOfData := (MemoryManager atAddress: va - SIZE_OF_HEADER) & _METHOD_EXTRACT_SIZE - SIZE_OF_HEADER.
	sizeOfData == 0 ifTrue: [^EmptyArray].
	answer := OrderedCollection new.
	ptr := va - sizeOfData.
	[sizeOfData > 0 ] whileTrue: [
		answer add: (Object _fromAddress: ptr).
		cb := ((MemoryManager atAddress: ptr - 4) & _EXTRACT_SIZE).
		ptr := ptr + cb.
		sizeOfData := sizeOfData - cb
	].
	^answer! !

!Core::MethodDescriptor .INTERNALDEV methods 07:27 - 03/17/00!
referencesLiteral: anObject
	"
	Answers whether the method includes a specified literal symbol.
	Parameters:
		anObject    An Object.
	Return Value: 
		A Boolean.
	"
	^(self allLiterals indexOfElement: anObject) ~~ 0! !

!Smalltalk class Enumerating methods 05:44 - 03/17/00!
allBehaviorsDo: aBlock
	"
	Evaluate aBlock with all behaviors (Classes and Metaclasses) in the system.
	Parameters: 
		aBlock    A one-argument block.
	Return Value: 
		This method does not return a value.
	"
	SmalltalkClassDictionary do: [ :eClass|
		aBlock value: eClass.
		aBlock value: eClass class.
	].! !

!Smalltalk class Testing methods 06:16 - 03/17/00!
isVariableBinding: aSymbol
	"
	Answers whether aSymbol binds a class or a global.
	Parameters: 
		aSymbol    A Symbol.
	Return Value: 
		A Boolean.
	"
	^(SmalltalkClassDictionary includesKey: aSymbol) or: 
		[SmalltalkGlobalDictionary includesKey: aSymbol]! !

!Class .INTERNALDEV methods 07:19 - 03/15/00!
classPool
	"
	Answers a Dictionary that associates class variable names with their
	values.
	Return Value: 	
		A Dictionary.
	"
	|dic vars |
	dic := Dictionary new.
	vars := (Compiler getClassDescriptor: self name) classVariables.
	1 to: vars size do: [ :i |
		dic at: (vars at: i) asSymbol put: (m_sharedPool at: i)
	].
	^dic! !

!Metaclass Accessing methods 05:50 - 03/17/00!
soleInstance
	"
	Answers the receiver.
	"
	^self! !

!Symbol Testing methods 06:16 - 03/17/00!
isVariableBinding
	"
	Answers whether aSymbol binds a class or a global.
	Parameters: 
		aSymbol    A Symbol.
	Return Value: 
		A Boolean.
	"
	^ Smalltalk isVariableBinding: self! !

!SystemTest * methods 07:30 - 03/15/00!
setUp
	ClassVar1 := #foo.
	ClassVar2 := 42.! !

!SystemTest * methods 07:30 - 03/15/00!
testclassPool
	self 
		assert: (self class classPool at: #ClassVar1) = #foo;
		assert: (self class classPool at: #ClassVar2) = 42! !

!SystemTest * methods 06:44 - 03/16/00!
testcurrentSelector
	self 
		assert: (self currentSelector) = #testcurrentSelector! !

!SystemTest * methods 06:50 - 03/17/00!
testliterals
	|method|
	method := self class compiledMethodAt: #testliterals.
'abc'.
'def'.
#('abc' 'def' '123' ('foo')).
	^method allLiterals! !

Compiler addCatDescription: 'Accessing' text: 'Accessing
Accesses properties of the object.'.
Compiler addCatDescription: '.INTERNALDEV' text: '.INTERNALDEV
This category marks methods that are not included in runtime images.'.
Compiler addCatDescription: '*' text: '*
Default category.'.
Compiler addCatDescription: 'Testing' text: 'Testing
Test the receiver and answer a Boolean.'.
Compiler addCatDescription: 'Enumerating' text: 'Enumerating
Enumerate elements.'.!
