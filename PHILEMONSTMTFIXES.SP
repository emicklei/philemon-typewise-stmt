""
PROJECTNAME PhilemonStMtFixes .
PROJECTCATEGORY  .

CLASS Point
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	/
	-
END
! !

!Point publicMethods methods 10:21 - 08/20/04!
- delta

	"Answer a new point that is the difference of the receiver and delta."

	^delta isPoint
		ifTrue: [Point x: (self x - delta x) y: (self y - delta y)]
		ifFalse: [Point x: (self x - delta) y: (self y - delta)]! !

!Point publicMethods methods 10:20 - 08/20/04!
/ scale

	"Answer a new point that is the quotient of the receiver and scale."

	^scale isPoint
		ifTrue: [Point x: (self x / scale x) y: (self y / scale y)]
		ifFalse: [Point x: (self x / scale) y: (self y / scale)]! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
