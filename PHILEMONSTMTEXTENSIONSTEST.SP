""
PROJECTNAME PhilemonStMtExtensionsTest .
PROJECTCATEGORY  .

CLASS TestCase
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	selector
END
! !

!TestCase publicMethods methods 01:06 - 03/02/04!
selector
	^self testMessage ifNil: [ 'Unknown' ]! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
