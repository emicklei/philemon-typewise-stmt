""
PROJECTNAME PhilemonColorSupportStMt .
PROJECTCATEGORY  .


PROFILE
BEGIN
END

CLASS Color
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS 
BEGIN
END
! !
RGB variableByteSubclass: #Color
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!

!Color class * methods 09:40 - 01/19/04!
black

	^self preDefined at: #black ifAbsentPut: [self redByte: 0 greenByte: 0  blueByte: 0]! !

!Color class * methods 09:40 - 01/19/04!
red: red green: green blue: blue

	^super
		red: (red < 256 ifTrue: [red * 256] ifFalse: [red])
		green: (green < 256 ifTrue: [green * 256] ifFalse: [green])
		blue: (blue < 256 ifTrue: [blue * 256] ifFalse: [blue])! !

!Color class * methods 09:40 - 01/19/04!
redByte: red greenByte: green blueByte: blue

	^self red: red * 256 green: green * 256 blue: blue * 256! !

!Color class * methods 09:40 - 01/19/04!
white

	^self preDefined at: #white ifAbsentPut: [self redByte: 255 greenByte: 255  blueByte: 255]! !

Compiler addCatDescription: '*'A text: '*
Default category.'A.!
