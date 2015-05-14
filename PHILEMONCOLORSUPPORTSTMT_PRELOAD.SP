""
PROJECTNAME "PhilemonColorSupportStMt Preload" .
PROJECTCATEGORY "Philemon Color Support" .


PROFILE
BEGIN
END

CLASS RGB
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	redByte
	blueByte
	greenByte
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

!RGB Accessing methods 12:31 - 03/09/04!
blueByte
	^self blue! !

!RGB Accessing methods 12:31 - 03/09/04!
greenByte
	^self green! !

!RGB Accessing methods 12:31 - 03/09/04!
redByte
	^self red! !

!Color class * methods 09:40 - 01/19/04!
black

	^self preDefined at: #black ifAbsentPut: [self redByte: 0 greenByte: 0  blueByte: 0]! !

!Color class publicMethods methods 03:34 - 03/02/04!
darkGray
	^self grey darker! !

!Color class publicMethods methods 03:34 - 03/02/04!
gray
	^self grey! !

!Color class publicMethods methods 03:34 - 03/02/04!
gray: intense
	^self grey: intense! !

!Color class publicMethods methods 03:34 - 03/02/04!
lightGray
	^self grey lighter! !

!Color class * methods 11:46 - 03/09/04!
red: r green: g blue: b
	^self value: (r | (g << 8) | (b << 16))! !

!Color class * methods 01:31 - 03/08/04!
redByte: red greenByte: green blueByte: blue

	^self red: red green: green blue: blue! !

!Color class * methods 09:40 - 01/19/04!
white

	^self preDefined at: #white ifAbsentPut: [self redByte: 255 greenByte: 255  blueByte: 255]! !

Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.!
