"release             
	Processor outputDebugLine: 'Object>>release called; should not happen!'."
PROJECTNAME PhilemonStMtExtensions .
PROJECTCATEGORY  .


PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	instSize
END
INSTANCEMETHODS 
BEGIN
	ifNil:
	ifNotNil:
	release
	echo
	echo:
END

CLASS Delay
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS Message
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	selector:arguments:
END
INSTANCEMETHODS 
BEGIN
END

CLASS Point
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	maxPoint:
	minPoint:
END

CLASS Rectangle
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	left:right:top:bottom:
	left:top:right:bottom:
	new
END
INSTANCEMETHODS 
BEGIN
	=
	leftCenter
	rightCenter
	topCenter
	bottomCenter
	center
	corner:
	origin
	origin:
END

CLASS Smalltalk
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	classAt:ifAbsent:
	classAt:
END
INSTANCEMETHODS 
BEGIN
END

CLASS UndefinedObject
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	ifNil:
	ifNotNil:
END

CLASS ContextBlock
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	repeat
	whenErrorDo:
	on:do:
	when:do:
END

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asMessageTo:
	asClass
	copyWith:
	occurrencesOf:
END

CLASS Character
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	cr
	tab
	lf
END
INSTANCEMETHODS 
BEGIN
	isDoubleByte
	isLetter
END

CLASS Time
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	newDay:monthIndex:year:
	fromMilliseconds:
END
INSTANCEMETHODS 
BEGIN
	monthIndex
	monthIndex:
	dayOfMonth
	dayOfMonth:
	asMilliseconds
	minutes
	minutes:
	seconds
	seconds:
	hours
	hours:
END

CLASS WinRectangle
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	union:
	translateBy:
END

CLASS Error
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS Array
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asByteArray
END

CLASS ByteArray
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	fromString:
END
INSTANCEMETHODS 
BEGIN
END

CLASS Interval
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asByteArray
END

CLASS OrderedCollection
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	insert:atIndex:
END

CLASS Dictionary
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	at:ifAbsentPutUsing:
END

CLASS HModule
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	tryLoadLibrary:
END
INSTANCEMETHODS 
BEGIN
END

CLASS EsRandom
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS Fraction
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	=
	floor
	ceiling
END

CLASS Integer
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	bitOr:
	bitAnd:
	floor
	ceiling
	truncateTo:
	clearBit:
	setBit:
END

CLASS ExError
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS String
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	startsWith:
END

CLASS IDE::WalkBackDialog
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	generateSourceForUndefinedMessage:
END
! !
Object subclass: #Delay
	instanceVariableNames: 'milliSecondsToWait '
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
WinException subclass: #Error
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
Random variableByteSubclass: #EsRandom
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
Error subclass: #ExError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!

!Object class * methods 02:19 - 02/02/04!
instSize
	^self sizeInBytes! !

!Object Printing methods 12:19 - 10/06/04!
echo
	Logger techInfo: self! !

!Object Operations methods 12:18 - 10/06/04!
echo: what
	Logger techInfo: [ self printString, ':', what printString ].! !

!Object Testing methods 01:25 - 02/26/04!
ifNil: aBlock
	""! !

!Object Testing methods 03:26 - 03/01/04!
ifNotNil: aBlock
	^aBlock value: self! !

!Object Allocating methods 10:38 - 10/05/04!
release             
	" Let garbage collector handle this "! !

!Delay class * methods 02:18 - 03/02/04!
forSeconds: seconds
	^self new timeToWait: (seconds * 1000) asInteger! !

!Delay * methods 02:17 - 03/02/04!
timeToWait: milliseconds
	milliSecondsToWait := milliseconds asInteger! !

!Delay * methods 02:15 - 03/02/04!
wait  
	self wait: milliSecondsToWait! !

!Delay * methods 02:17 - 03/02/04!
wait: millis    
	Processor currentThread wait: millis! !

!Message class * methods 02:19 - 02/02/04!
selector: aSymbol arguments: anArray
	^self new receiver: nil selector: aSymbol arguments: anArray; yourself.! !

!Point publicMethods methods 11:59 - 03/29/04!
maxPoint: operand
	
	" StMt implements Point>>max: incorrect and 'primitive'. 
	  this method should be used "
 	| maxX maxY |
 	maxX := self x max: operand x. 
	maxY := self y max: operand y.
	^self class x: maxX y: maxY! !

!Point publicMethods methods 11:59 - 03/29/04!
minPoint: operand
	 
	" StMt implements Point>>max: incorrect and 'primitive'. 
	  this method should be used "
 
 	| minX minY |
 	minX := self x min: operand x. 
	minY := self y min: operand y.
	^self class x: minX y: minY! !

!Rectangle class Allocating methods 02:16 - 02/02/04!
left: left right: right top: top bottom: bottom
	"
	Answers an instance of the receiver initialized with the specified coordinates.
	Parameters:
		left     An Integer with the upper left x-coordinate.
		top      An Integer with the upper left y-coordinate.
		right    An Integer with the lower right x-coordinate.
		bottom   An Integer with the lower right y-coordinate.
	Return Value:
		An instance of the receiver.
	"
	^self left: left top: top right: right bottom: bottom! !

!Rectangle class Allocating methods 02:19 - 02/02/04!
left: left top: top right: right bottom: bottom
	"
	Answers an instance of the receiver initialized with the specified coordinates.
	Parameters:
		left     An Integer with the upper left x-coordinate.
		top      An Integer with the upper left y-coordinate.
		right    An Integer with the lower right x-coordinate.
		bottom   An Integer with the lower right y-coordinate.
	Return Value:
		An instance of the receiver.
	"
	| cls |
	left isSmallInteger 
		&& right isSmallInteger 
		&& top isSmallInteger 
		&& bottom isSmallInteger 
	ifTrue: [ cls := WinRectangle ] 
	ifFalse: [ cls := Rectangle ].
	^(cls new) left: left top: top right: right bottom: bottom! !

!Rectangle class Allocating methods 01:30 - 08/20/04!
new
	"
	Creates an instance of the receiver with zero coordinates.
	Return Value:
		A Rectangle.
	"
	^(self basicNew: 4)! !

!Rectangle Testing methods 02:19 - 02/02/04!
= operand

	"Answer true if the receiver and operand are equal, and false otherwise."

	^operand isRectangle and: [(self right = operand right) and: [(self left = operand left) and: [(self top = operand top) and: [self bottom = operand bottom]]]]! !

!Rectangle Operations methods 12:36 - 03/09/04!
bottomCenter

	"Answer a point at the center of the receiver's top horizontal line."

	^((self origin x + self corner x) // 2) @ self corner y! !

!Rectangle Operations methods 11:38 - 08/20/04!
center
	" Round this to prevent future trouble "
	^self origin + (self extent / 2) ceiling! !

!Rectangle Accessing methods 01:44 - 08/20/04!
corner: newPoint
	self right: newPoint x.
	self bottom: newPoint y.! !

!Rectangle Operations methods 09:36 - 03/09/04!
leftCenter

	"Answer a point at the center of the receiver's left vertical line."

	^self origin x @ ((self origin y + self corner y) // 2)! !

!Rectangle Accessing methods 01:44 - 08/20/04!
origin
	^Point x: self left y: self top! !

!Rectangle Accessing methods 01:45 - 08/20/04!
origin: newPoint
	self left: newPoint x.
	self top: newPoint y.! !

!Rectangle Operations methods 09:36 - 03/09/04!
rightCenter

	"Answer a point at the center of the receiver's left vertical line."

	^self corner x @ ((self origin y + self corner y) // 2)! !

!Rectangle Operations methods 12:36 - 03/09/04!
topCenter

	"Answer a point at the center of the receiver's top horizontal line."

	^((self origin x + self corner x) // 2) @ self origin y! !

!Smalltalk class Accessing methods 07:27 - 01/29/04!
classAt: aSymbol
	^self at: aSymbol! !

!Smalltalk class Accessing methods 07:27 - 01/29/04!
classAt: aSymbol ifAbsent: aBlock
	^self at: aSymbol ifAbsent: aBlock.! !

!UndefinedObject Testing methods 01:24 - 02/26/04!
ifNil: aBlock
	^aBlock value! !

!UndefinedObject Testing methods 01:38 - 03/01/04!
ifNotNil: aBlock
	""! !

!ContextBlock Operations methods 09:11 - 03/09/04!
on: anException do: aBlockToExecute
	^self try: self filter: [
		:exCode :exPointers | 
			((WinException exceptionCode: exCode exceptionPointers: exPointers) = anException) 
				ifTrue: [ EXCEPTION_EXECUTE_HANDLER ] 
				ifFalse: [ EXCEPTION_CONTINUE_SEARCH ] 
	] except: aBlockToExecute! !

!ContextBlock Operations methods 01:34 - 03/01/04!
repeat
	"Evaluate the receiver forever"

	[true]
		whileTrue:
			[self value]! !

!ContextBlock Operations methods 09:09 - 03/16/04!
when: anException do: aBlock
	^self on: anException do: aBlock! !

!ContextBlock Operations methods 02:47 - 03/01/04!
whenErrorDo: aBlock
	self try: self except: [ aBlock value: Error new ] .! !

!Symbol Operations methods 12:25 - 03/12/04!
asClass
	^Smalltalk at: self! !

!Symbol Operations methods 09:16 - 03/09/04!
asMessageTo: aReceiver
	^Message new receiver: aReceiver selector: self! !

!Symbol Operations methods 09:14 - 03/16/04!
copyWith: aPostFix
	" Quick and dirty implementation, 
		have some doubts if this will work in runtime environment where
		symbols are integer identifiers instead of strings.. "
	^(self asString copyWith: aPostFix) asSymbol! !

!Symbol Operations methods 11:59 - 03/16/04!
occurrencesOf: aCharacter
	^self asString occurrencesOf: aCharacter! !

!Character class publicMethods methods 09:51 - 03/10/04!
cr
	^self value: 13! !

!Character class publicMethods methods 09:54 - 03/10/04!
lf
	^self value: 10! !

!Character class publicMethods methods 02:28 - 03/02/04!
tab
	^self value: 9! !

!Character Testing methods 10:07 - 05/07/04!
isDoubleByte
	^self sizeInBytes == 2! !

!Character Testing methods 01:31 - 07/22/04!
isLetter
	^self isAlpha! !

!Time class Allocating methods 07:27 - 01/29/04!
fromMilliseconds: milliSeconds
	| instance usedMillis |
	instance := self new. 
	instance hour: ((milliSeconds / (60*60*1000)) asFloat truncateTo: 1).
	usedMillis := instance hour * 60*60*1000.
	instance minute: (((milliSeconds-usedMillis) / (60*1000)) asFloat truncateTo: 1). 
	usedMillis := usedMillis + (instance minute *60*1000).
	instance second: (((milliSeconds-usedMillis) / 1000) asFloat truncateTo: 1). 
	usedMillis := usedMillis + (instance second * 1000).
	instance milliseconds: (milliSeconds-usedMillis).
	^instance.! !

!Time class Allocating methods 07:27 - 01/29/04!
newDay: day monthIndex: month year: year
	^self new day: day; month: month; year: year; yourself! !

!Time Accessing methods 07:27 - 01/29/04!
asMilliseconds
	^(((((self hour *60) + self minute) * 60) + self second) * 1000) + self milliseconds.! !

!Time Accessing methods 07:27 - 01/29/04!
dayOfMonth
	^self day! !

!Time Accessing methods 07:27 - 01/29/04!
dayOfMonth: anInteger
	^self day: anInteger! !

!Time Accessing methods 02:19 - 02/02/04!
hours
	"
	Answers the hour.
	Return Value:
		The hour in integer format.
	"
	^self hour! !

!Time Accessing methods 02:19 - 02/02/04!
hours: anInteger
	"
	Answers the hour.
	Return Value:
		The hour in integer format.
	"
	^self hour: anInteger! !

!Time Accessing methods 02:19 - 02/02/04!
minutes
	"
	Answers the minute.
	Return Value:
		The minute in integer format.
	"
	^self minute! !

!Time Accessing methods 02:19 - 02/02/04!
minutes: anInteger
	"
	Answers the minute.
	Return Value:
		The minute in integer format.
	"
	self minute: anInteger! !

!Time Accessing methods 07:27 - 01/29/04!
monthIndex
	^self month.! !

!Time Accessing methods 07:27 - 01/29/04!
monthIndex: anInteger
	^self month: anInteger! !

!Time Accessing methods 02:19 - 02/02/04!
seconds
	"
	Answers the second.
	Return Value:
		The second in integer format.
	"
	^self second! !

!Time Accessing methods 02:19 - 02/02/04!
seconds: anInteger
	"
	Answers the second.
	Return Value:
		The second in integer format.
	"
	^self second: anInteger! !

!WinRectangle Operations methods 03:56 - 03/19/04!
translateBy: delta

	self origin: self origin + delta.
	self corner: self corner + delta.! !

!WinRectangle Operations methods 03:12 - 03/11/04!
union: operand

	"Answer a rectangle which is the area in which the receiver overlaps with operand."

	^(self origin max: operand origin)
		corner: (self corner min: operand corner)! !

!Error * methods 02:53 - 03/01/04!
exitWith: anObject
	" Stub: this method should execute a block with parameter anObject but since this is not supported, 
	we just return anObject. If you ever plan to use this, please implement this method. "
	^anObject! !

!Array Converting methods 07:27 - 01/29/04!
asByteArray
	"
	Answers a ByteArray with the elements of the receiver.
	Return Value:
		A ByteArray.
	Remarks:
	This method is optional. It is reimplemented in Array and OrderedCollection with better efficiency.
	"
	| array j |
	array := ByteArray new: self size.
	j := 0.
	self do: [ :e | array at: (j := j _asInteger + 1) put: e ].
	^array! !

!ByteArray class publicMethods methods 11:47 - 02/26/04!
fromString: aString
	" in StMt, a String is a ByteArray "
	" BUT for the use of BOA, this really should be a ByteArray and NOT a String"
	^(ByteArray new: 0), aString! !

!Interval Converting methods 07:27 - 01/29/04!
asByteArray
	"
	Answers a ByteArray with the elements of the receiver.
	Return Value:
		A ByteArray.
	Remarks:
	This method is optional. It is reimplemented in Array and OrderedCollection with better efficiency.
	"
	| array j |
	array := ByteArray new: self size.
	j := 0.
	self do: [ :e | array at: (j := j _asInteger + 1) put: e ].
	^array! !

!OrderedCollection Operations methods 02:56 - 03/01/04!
insert: anObject atIndex: index
	" Wrapper method for VASt - StMt "
	^self insert: anObject at: index! !

!Dictionary Accessing methods 10:01 - 03/30/04!
at: aKey ifAbsentPutUsing: absentBlock
	"
	Answers the value at aKey. If the key is not present, stores the result
	of evaluating absentBlock at the specified key and returns that value.
	Parameters:
		aKey         An Object used as the key to look for. aKey must not be nil.
		absentBlock  A Block that is evaluated if the key cannot be found.
	Return Value:
		The value, an Object, stored at the specified key.
	"
	^self at: aKey ifAbsentPut: absentBlock! !

!HModule class Accessing methods 02:04 - 09/09/04!
tryLoadLibrary: szFileName
	"
	Loads the specified module or returns nil if not loaded
	be loaded.
	Parameters:
		szFileName	File name of the module to load.
	Return Value:
		An instance of the receiver or nil.
	"
	| hModule |
	hModule := WINAPI LoadLibrary: szFileName.
	hModule ~~ NULL ifTrue: [^self value: hModule].
	_DEBUG_USER == TRUE ifTrue: [
		MessageBox printLastError: szFileName.
	].
	^nil! !

!Fraction Testing methods 10:43 - 03/01/04!
= aNumber
	"
	Compares the receiver to aNumber and answers a boolean.
	Parameters:
		aNumber	A Float, or a Number which is converted to a float.
	Return Value: 
		A Boolean.
	"
	(aNumber isMemberOf: Fraction) ifTrue: [
		^(numerator = aNumber numerator) && (denominator = aNumber denominator)
	].
	aNumber isNumber ifTrue: [
		^aNumber asFloat = self asFloat
	] ifFalse: [ ^false ].! !

!Fraction Operations methods 12:54 - 03/09/04!
ceiling
	^self asFloat ceiling! !

!Fraction Operations methods 12:54 - 03/09/04!
floor
	^self asFloat floor! !

!Integer Operations methods 08:48 - 01/23/04!
bitAnd: anInteger
	^self & anInteger! !

!Integer Operations methods 03:53 - 03/02/04!
bitOr: aNumber
	^self | aNumber! !

!Integer Operations methods 12:54 - 03/09/04!
ceiling
	^self! !

!Integer Operations methods 01:01 - 09/02/04!
clearBit: bitPosition
	^((bitPosition - 1 power: 2) bitXor: -1) bitAnd: self! !

!Integer Operations methods 12:53 - 03/09/04!
floor
	^self! !

!Integer Operations methods 01:04 - 09/02/04!
setBit: bitPosition
	^(bitPosition - 1 power: 2) bitOr: self! !

!Integer Operations methods 10:04 - 03/30/04!
truncateTo: aNumber
	" Should implement this for integers "
	^self asFloat truncateTo: aNumber! !

!String Testing methods 07:16 - 10/14/04!
startsWith: aString
	^self beginsWith: aString! !

!IDE::WalkBackDialog Private methods 09:29 - 03/08/04!
generateSourceForUndefinedMessage: message    
	"Returns a String representing source code for the given message"
	
	"for unary messages return just the name"
	|word idx coll source stream|
	message selector numberOfArguments == 0 ifTrue: [^message selector asString,'\n\n\tself halt'].
	  
	"binary message"  
	(message selector asString includes: $:) ifFalse: [
		^message selector asString,' ',(self prefixedClassNameFor: message arguments first class),'\n\n\tself halt'
	].
	
	"for keyword messages generate appropriate method code"
	stream := Stream on: message selector asString. 	
	source := Stream on: ''. 
	
	"build collection with class names (with a correct prefix)"
	coll := message arguments
				inject: OrderedCollection new
				into: [:cls :each | | clsName | clsName := (self prefixedClassNameFor: each class). 
						(cls includes: clsName)  
				       		ifFalse: [ cls add: clsName ] 
				       		ifTrue: [ cls add: clsName,(cls size asString)]. 
				       	cls].                                   
				
	"merge keywords and generated type names"	
	idx := 1.  
	[stream atEnd] 
		whileFalse: [
			word := stream upTo: $:.
			
			source
				nextPutAll: word;
				nextPutAll: ': ';
				nextPutAll: (coll at: idx);
				nextPutAll: ' '.
			idx := idx + 1.	
	].                	
	^source contents,'\n\n\tself halt'! !

Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'Converting'A text: 'Converting
Convert the receiver to another type.'A.
Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Printing'A text: 'Printing
Print a textual representation of the receiver.
'A.
Compiler addCatDescription: 'Private'A text: 'Private
Private methods.'A.
Compiler addCatDescription: 'Testing'A text: 'Testing
Test the receiver and answer a Boolean.'A.
Compiler addCatDescription: 'Allocating'A text: 'Allocates a new object.'A.!
