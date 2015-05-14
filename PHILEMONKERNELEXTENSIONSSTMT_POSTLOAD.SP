""
PROJECTNAME "PhilemonKernelExtensionsStMt Postload" .
PROJECTCATEGORY "Philemon Kernel Extensions" .
PREREQUISITES philemonstmtextensions smalltalk "smalltalk extensions" "smalltalk system extensions" .

CLASS Point
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	theta
END

CLASS Symbol
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	argumentCount
END

CLASS Number
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	arcTanWithDenominator:
	roundTo:
END

CLASS OrderedCollection
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	add:afterIndex:
END

CLASS Integer
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asBinary
	asHex
END
! !

!Point publicMethods methods 07:21 - 03/25/04!
theta

	"Answer the angle the receiver makes with origin in radians.   
	right is 0; up is (Pi / 2) radians (90 degrees).  Result will be
	between 0 and (2 * Pi) radians"   
	
	"Changed from VAST because of nested returns; known bug in MT5"

	| tan theta pi |
	self x = 0
		ifTrue: 
			[ pi := Float pi.
			self y >= 0
				ifTrue: [ ^(pi * 0.5) ]
				ifFalse: [ ^(pi * 1.5) ] ]
		ifFalse: 
			[ tan := self y asFloat / self x asFloat.
			theta := tan arcTan.
			self x < 0
				ifTrue: 
					[ ^(theta + theta class pi) ]
				ifFalse: 
					[ theta < 0
						ifTrue: [ ^(theta + ( theta class pi * 2 )) ]
						ifFalse: [ ^theta ] ] ]! !

!Symbol Operations methods 02:12 - 04/23/04!
argumentCount
	^self numberOfArguments! !

!Number publicMethods methods 10:12 - 02/25/04!
arcTanWithDenominator: denominator 

	"Answer the arctan of the receiver within a range of -pi to pi dependant of the sign of denominator"
	
	| signOfDenominator signOfNominator |                      
	signOfNominator := 1.
	signOfDenominator := denominator sign.
	signOfNominator := signOfDenominator * self sign. 

	signOfDenominator = 0 
		ifTrue:
			[ signOfNominator = 1 
				ifTrue:[ ^Float pi / 2 ].
			^ Float pi / 2 negated ]. 

	signOfDenominator = 1
		ifTrue:
			[ ^self asFloat arcTan]
		ifFalse:
			[ signOfNominator = -1
				ifTrue:[ ^ self asFloat arcTan - Float pi ]
				ifFalse:[ ^ self asFloat arcTan + Float pi ] ]! !

!Number Operations methods 10:16 - 02/25/04!
roundTo: aNumber

	"Answer a type of Number, which is the multiple
	 of the argument, aNumber, that is nearest the
	 receiver. Note that for floating point numbers,
	 the result is not exact due to rounding.

	 Fail if aNumber is not a type of Number.
	 Fail if aNumber is zero."

	"The rounding (which normally occurs to the next highest
	 multiple of aNumber) goes down  instead. The reason
	 is due to the inexact nature of floats (for example,
	 4.5 roundTo: 0.2 = 4.4 instead of 4.6 due to loss of
	 precision, 4.500000000001 roundTo: 0.2 = 4.6 however).

	 Note: one solution (very slow however) is to scale
	 the receiver and aNumber by a constant x such that
	 both numbers are integers. Then apply the roundTo:
	 and scale back down. Obviously, for inexact numbers
	 (such as 2.3=2.29999999...) what is this scale factor?"

	 ^(self + (self sign * (aNumber/2 ) abs))
								 truncateTo: aNumber! !

!OrderedCollection Operations methods 12:55 - 03/05/04!
add: anObject afterIndex: index
	" Wrapper method for VASt - StMt "
	^self insert: anObject at: index+1! !

!Integer publicMethods methods 10:25 - 02/25/04!
asBinary
	^'2r', (self printStringRadix: 2)! !

!Integer publicMethods methods 11:19 - 02/23/04!
asHex
	^'16r', (self printStringRadix: 16)! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.!
