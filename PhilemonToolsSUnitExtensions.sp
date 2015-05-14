"Philemon SmalltalkMT exporter has done all the work"
PROJECTNAME PhilemonToolsSUnitExtensions .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	sunitTestClass
END
INSTANCEMETHODS
BEGIN
END

CLASS TestCase
INCLUDEDEF=0
CLASSMETHODS
BEGIN
	sunitTestClass
END
INSTANCEMETHODS
BEGIN
	is:value:equalTo:value:
	is:value:equalTo:value:debug:
	is:value:equalTo:
END

CLASS Teachable
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	whenReceived:evaluate:
	whenReceived:return:
	learnings
	doesNotUnderstand:
	acceptReceipt:
END

! !

Object subclass: #Teachable
    instanceVariableNames: 'learnings '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!Object class publicMethods !

sunitTestClass
	"Answer the class containing tests for this class
	or nil if no such class exists. Subclasses may
	redefine this for specfic test cases"
	
	^self applications first sunitTestClass! !
	

!TestCase class publicMethods !

sunitTestClass
	"That's me"! !

!TestCase publicMethods !

is: label value: itsValue equalTo: otherValue

	self assert: (
		itsValue = otherValue
			ifFalse:[ 
				Transcript cr.
				label printOn: Transcript.
				Transcript nextPutAll: ': '.
				itsValue printOn: Transcript.
				Transcript nextPutAll: ' ?= '.
				otherValue printOn: Transcript.
				Transcript cr]
		;yourself)!

is: label value: itsValue equalTo: label2 value: otherValue

	self assert: (
		itsValue = otherValue
			ifFalse:[ 
				Transcript cr.
				label printOn: Transcript.
				Transcript nextPutAll: ': '.
				itsValue printOn: Transcript.
				Transcript nextPutAll: ' ?= '.
				label2 printOn: Transcript.
				Transcript nextPutAll: ': '.
				otherValue printOn: Transcript.
				Transcript cr]
		;yourself)!

is: label value: itsValue equalTo: label2 value: otherValue debug: debugPrintable
	self assert: (
		itsValue = otherValue
			ifFalse:[ 
				Transcript cr.
				label printOn: Transcript.
				Transcript nextPutAll: ': '.
				itsValue printOn: Transcript.
				Transcript nextPutAll: ' ?= '.
				label2 printOn: Transcript.
				Transcript nextPutAll: ': '.
				otherValue printOn: Transcript.
				Transcript cr; tab.
				debugPrintable printOn: Transcript.
				Transcript cr]
		;yourself)! !
	

!Teachable publicMethods !

acceptReceipt: aSymbol
	"Teach the receiver to return self if the given message is sent to him."
	self learnings at: aSymbol put: (Array with: #return with: self)!

doesNotUnderstand: aMessage 

	"This method is sent whenever the receiver does not have a method
   	to match the receiver. The receiver checks's if he was teached to
	handle the method and runs the appropriate action."

	| entry |
	entry := self learnings 
		at: aMessage selector 
		ifAbsent: [ ^self error: aMessage ].
	#return == ( entry at: 1 ) 
		ifTrue: [ ^entry at: 2 ].
	#zeroBlock == ( entry at: 1 ) 
		ifTrue: [ ^( entry at: 2 ) value ]!

learnings

	learnings isNil ifTrue: [ learnings := IdentityDictionary new ].
	^learnings!

whenReceived: aSelector evaluate: aBlock
	"Teach the receiver to evaluate the given block if a message with
	 the given selector is called."
	self learnings at: aSelector put: (Array with: #zeroBlock with: aBlock)!

whenReceived: aSymbol return: anObject
	"Teach the receiver to return anObject if a message with the
	given selector is called. The selector can specify a unary,
	binary and keyword message."
	self learnings at: aSymbol put: (Array with: #return with: anObject)! !


