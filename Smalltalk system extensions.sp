""
PROJECTNAME "smalltalk system extensions" .
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
	listAtCategoryNamed:
	categories
	organization
	categoryOfElement:
	whichSelectorsReferTo:
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
	allClasses
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

CLASS MappingTable
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	removeKey:ifAbsent:
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

!Object * methods 07:07 - 03/16/00!
currentMethodSelector
	|ret|
	ret := _asm{ mov eax, [ebp+4] }.
	(ret := Exception methodFromVA: ret basicAddress) notNil ifTrue: [
		^ret at: 2
	].
	^nil! !

!Object * methods 06:44 - 03/16/00!
currentSelector
	|ret|
	ret := _asm{ mov eax, [ebp+4] }.
	(ret := Exception methodFromVA: ret basicAddress) notNil ifTrue: [
		^ret at: 2
	].
	^nil! !

!Behavior * methods 05:52 - 04/20/00!
categories
	^self methodDictionary collect: [ :e | e category ].! !

!Behavior * methods 10:25 - 04/20/00!
categoryOfElement: aSelector
	^(self compiledMethodAt: aSelector) category! !

!Behavior * methods 05:44 - 03/17/00!
compiledMethodAt: aSymbol
	^self methodDictionary at: aSymbol asString! !

!Behavior * methods 05:52 - 04/20/00!
listAtCategoryNamed: aProtocol
	^self methodDictionary select: [ :e | e category = aProtocol].! !

!Behavior * methods 10:25 - 04/20/00!
organization
	^self! !

!Behavior * methods 12:39 - 03/15/00!
sourceCodeAt: aSymbol
	^(self methodDictionary at: aSymbol asString) source! !

!Behavior * methods 10:25 - 04/20/00!
whichSelectorsReferTo: aLiteral
	| answer |
	answer := OrderedCollection new.
	self methodDictionary do: [ :selectorString :methodDescriptor |
		methodDescriptor allLiterals do: [ :eLiteral |
			aLiteral == eLiteral ifTrue: [
				answer add: selectorString
			]
		]
	]. 
	^answer collect: [ :e | e asSymbol ]! !

!Core::MethodDescriptor * methods 10:37 - 05/21/02!
allLiterals
	|va answer sizeOfData ptr cb obj|
	(va := self smalltalkClass instanceMethodAddressAt: m_methodID) == 0 ifTrue: [^EmptyArray].
	answer := OrderedCollection new.
	sizeOfData := (MemoryManager atAddress: va - SIZE_OF_HEADER) & _METHOD_EXTRACT_SIZE - SIZE_OF_HEADER.
	sizeOfData ~~ 0 ifTrue: [
		"Processor outputDebugLine: self printString."
		ptr := va - sizeOfData.
		cb := -1.
		(MemoryManager atAddress: va - SIZE_OF_HEADER) & _METHOD_SVA ~~ 0 ifTrue: [
			ptr := ptr + 4.
			sizeOfData := sizeOfData - 4
		].
		
		[(sizeOfData > 0) && (cb ~~ 0) ] whileTrue: [
			"Processor outputDebugLine: 'ptr=0x%08x sizeOfData=%d' _with: ptr _with: sizeOfData."
			obj := Object _fromAddress: ptr.
			(obj isKindOf: StaticBlock) ifTrue: [obj := nil].		
			obj isNil 
			ifTrue: [cb := 0]
			ifFalse: [
				answer add: obj.
				cb := (MemoryManager atAddress: ptr - 4) & _EXTRACT_SIZE.
				cb := cb alignOn: DATA_ALIGNMENT.
				ptr := ptr + cb.
				sizeOfData := sizeOfData - cb
			].
		].
	].
	
	_gFrameLiterals notNil ifTrue: [
		1 to: _gFrameLiterals size by: 4 do: [ :i |
			((_gFrameLiterals _at: i + 3) == m_methodID) &&
			((_gFrameLiterals _at: i + 1) == (Class fromName: m_class) classID) &&
			((_gFrameLiterals _at: i + 2) == m_isClassMethod) ifTrue: [
				answer add: (_gFrameLiterals _at: i)
			]
		]
	].
	^answer! !

!Core::MethodDescriptor * methods 07:27 - 03/17/00!
referencesLiteral: aSymbol
	^self allLiterals includes: aSymbol! !

!Smalltalk class * methods 05:44 - 03/17/00!
allBehaviorsDo: aBlock
	SmalltalkClassDictionary do: [ :eClass|
		aBlock value: eClass.
		aBlock value: eClass class.
	].! !

!Smalltalk class * methods 09:22 - 05/22/00!
allClasses
	^SmalltalkClassDictionary values! !

!Smalltalk class * methods 06:16 - 03/17/00!
isVariableBinding: aSymbol
	^(SmalltalkClassDictionary includesKey: aSymbol) or: 
		[SmalltalkGlobalDictionary includesKey: aSymbol]! !

!Class * methods 07:19 - 03/15/00!
classPool
	|dic vars |
	dic := Dictionary new.
	vars := (Compiler getClassDescriptor: self name) classVariables.
	1 to: vars size do: [ :i |
		dic at: (vars at: i) asSymbol put: (m_sharedPool at: i)
	].
	^dic
	"	inject: Dictionary new
		into: [ :dict :var |
			dict 
			at: var asSymbol put: (m_sharedPool at: (;
			yourself]"! !

!Metaclass * methods 05:50 - 03/17/00!
soleInstance
	^self! !

!MappingTable * methods 09:22 - 05/22/00!
removeKey: aKey ifAbsent: aBlock
	|answer|
	(answer := self removeKey: aKey) isNil ifTrue: [
		answer := aBlock value
	].
	^answer! !

!Symbol * methods 06:16 - 03/17/00!
isVariableBinding
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

Compiler addCatDescription: '*'A text: '*
Default category.'A.!
