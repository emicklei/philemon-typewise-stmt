""
PROJECTNAME PhilemonLogger .
PROJECTCATEGORY  .

CLASS Object
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	debugString
END

CLASS HomeContext
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	debugString
END

CLASS Logger
INCLUDEDEF=1
CLASSMETHODS 
BEGIN
	debug:
	disable
	enable
	output:
	level
	info:
	output:forLevel:
	techinfo:
	warn:
	error:
	warning:
	setDebug
	setTechInfo
	setInfo
	setWarn
	setError
	showError:
	techInfo:
END
INSTANCEMETHODS 
BEGIN
END
! !
Object subclass: #Logger
	instanceVariableNames: ''
	classVariableNames: 'level '
	poolDictionaries: ''
	comment: ''
	category: ''!

!Object publicMethods methods 10:50 - 10/04/04!
debugString
	^self asString! !

!HomeContext Operations methods 10:50 - 10/04/04!
debugString
	^self value asString! !

!Logger class output methods 12:02 - 10/06/04!
debug: aStringOrBlockOrObject              
	self output: aStringOrBlockOrObject forLevel: 5.! !

!Logger class * methods 09:58 - 10/04/04!
disable
	level := 0! !

!Logger class * methods 12:04 - 10/06/04!
enable
	self setDebug! !

!Logger class output methods 12:04 - 10/06/04!
error: aStringOrBlockOrObject
	self output: aStringOrBlockOrObject forLevel: 1.! !

!Logger class output methods 12:01 - 10/06/04!
info: aStringOrBlockOrObject
	self output: aStringOrBlockOrObject forLevel: 3.! !

!Logger class * methods 10:21 - 10/04/04!
level
	level isNil ifTrue: [ self disable ].
	^level.! !

!Logger class output methods 09:58 - 10/04/04!
output: aString
	Processor outputDebugLine: aString.! !

!Logger class output methods 12:02 - 10/06/04!
output: aStringOrBlockOrObject forLevel: aLevel
	self level < aLevel ifTrue: [ ^self ].
	self output: aStringOrBlockOrObject debugString.! !

!Logger class * methods 12:04 - 10/06/04!
setDebug
	level := 5! !

!Logger class * methods 12:05 - 10/06/04!
setError
	level := 1! !

!Logger class * methods 12:05 - 10/06/04!
setInfo
	level := 3! !

!Logger class * methods 12:05 - 10/06/04!
setTechInfo
	level := 4! !

!Logger class * methods 12:05 - 10/06/04!
setWarn
	level := 2! !

!Logger class output methods 12:11 - 10/06/04!
showError: anObject
	self error: anObject.! !

!Logger class output methods 12:15 - 10/06/04!
techinfo: aStringOrBlockOrObject
	self techInfo: aStringOrBlockOrObject! !

!Logger class output methods 12:15 - 10/06/04!
techInfo: aStringOrBlockOrObject
	self output: aStringOrBlockOrObject forLevel: 4.! !

!Logger class output methods 12:03 - 10/06/04!
warn: aStringOrBlockOrObject
	self output: aStringOrBlockOrObject forLevel: 2.! !

!Logger class output methods 12:03 - 10/06/04!
warning: aStringOrBlockOrObject
	self warn: aStringOrBlockOrObject! !

Compiler addCatDescription: 'output'A text: 'output'A.
Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.!
