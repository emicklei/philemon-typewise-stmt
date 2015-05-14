"StMt extensions for PhilemonKernelExtensions."
PROJECTNAME "PhilemonKernelExtensionsStMt Preload" .
PROJECTCATEGORY "Philemon Kernel Extensions" .


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
	subclassResponsibility
	species
	perform:
END

CLASS Point
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	<
	<=
	=
	>
	>=
	*
	/
	+
	-
END

CLASS Rectangle
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	origin:extent:
END
INSTANCEMETHODS 
BEGIN
END

CLASS Stream
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	print:
END

CLASS SequenceableCollection
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	doWithIndex:
	from:to:doWithIndex:
END

CLASS ContextBlock
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	applyWithIndex:from:to:
END

CLASS StaticBlock
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	applyWithIndex:from:to:
END

CLASS Number
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	newFromString:
END
INSTANCEMETHODS 
BEGIN
	degreesToRadians
	roundTo:
	sign
END

CLASS WinPoint
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	rotatedBy:
END

CLASS IdentityDictionary
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	at:ifAbsentPutUsing:
END

CLASS Interval
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	species
END

CLASS Float
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	pi
END
INSTANCEMETHODS 
BEGIN
	reciprocal
END

CLASS Fraction
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	reciprocal
END

CLASS Integer
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	bitAnd:
	power:
	reciprocal
END

CLASS WordArray
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	decodeFrom:from:startingAt:into:
	fromPrintableString:
END
INSTANCEMETHODS 
BEGIN
	asPrintableString
	compressedStoreOn:
	encodeFrom:into:startingAt:
END

CLASS String
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	fromBytes:
END
INSTANCEMETHODS 
BEGIN
	readStream
	writeStream
END

CLASS LargeInteger
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	arcTan
END

CLASS SmallInteger
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	arcTan
END
! !

!Object Operations methods 12:39 - 01/23/04!
perform: aSymbol 
	"
	Sends the unary message defined by aSymbol to the
	receiver.	Raises an exception if aSymbol does not represent an
	unary message.
	Parameters:
		aSymbol	A Symbol representing an unary message.
	Return Value:
		Depends on the message.
	"
	(aSymbol isMemberOf: Symbol) ifTrue: [
		aSymbol numberOfArguments == 0 ifTrue: [
			^self _perform: aSymbol
		].
		self raiseException: ST_EXCEPTION_INVALID_NUMBER_OF_ARGUMENTS
	].
	^self raiseException: ST_EXCEPTION_INVALID_ARGUMENT! !

!Object Copying methods 10:36 - 01/23/04!
species
	^self class! !

!Object Exception handling methods 09:55 - 01/23/04!
subclassResponsibility
	^self implementedBySubclass! !

!Point publicMethods methods 10:36 - 01/23/04!
- delta

	"Answer a new point that is the difference of the receiver and delta."

	^delta isPoint
		ifTrue: [Point x: (self x - delta x) y: (self y - delta y)]
		ifFalse: [Point x: (self x - delta) y: (self y - delta)]! !

!Point publicMethods methods 09:55 - 01/23/04!
* scale

	"Answer a new point that is the product of the receiver and scale."

	^scale isPoint
		ifTrue: [Point x: (self x * scale x) y: (self y * scale y)]
		ifFalse: [Point x: (self x * scale) y: (self y * scale)]! !

!Point publicMethods methods 09:55 - 01/23/04!
/ scale

	"Answer a new point that is the quotient of the receiver and scale."

	^scale isPoint
		ifTrue: [Point x: (self x / scale x) y: (self y / scale y)]
		ifFalse: [Point x: (self x / scale) y: (self y / scale)]! !

!Point publicMethods methods 10:36 - 01/23/04!
+ delta

	"Answer a new point that is the sum of the receiver and delta."

	^delta isPoint
		ifTrue: [Point x: (self x + delta x) y: (self y + delta y)]
		ifFalse: [Point x: (self x + delta) y: (self y + delta)]! !

!Point Testing methods 12:24 - 01/22/04!
< operand

	"Answer true if the receiver is above and to the left of operand. Otherwise
	 answer false."

	^self x < operand x and: [self y < operand y]! !

!Point Testing methods 12:24 - 01/22/04!
<= operand

	"Answer true if the receiver is neither below nor to the right of operand. Otherwise
	 answer false."

	^self x <= operand x and: [self y <= operand y]! !

!Point Testing methods 12:24 - 01/22/04!
= operand

	"Answer true if the receiver and operand are equal, and false otherwise."

	^operand isPoint and: [(self x = operand x) and: [self y = operand y]]! !

!Point Testing methods 12:24 - 01/22/04!
> operand

	"Answer true if the receiver is below and to the right of operand. Otherwise
	 answer false."

	^self x > operand x and: [self y > operand y]! !

!Point Testing methods 12:24 - 01/22/04!
>= operand

	"Answer true if the receiver is neither above nor to the left of operand. Otherwise
	 answer false."

	^self x >= operand x and: [self y >= operand y]! !

!Rectangle class Allocating methods 03:15 - 03/03/04!
origin: originPoint extent: extentPoint
	^self origin: originPoint corner: originPoint + extentPoint! !

!Stream Operations methods 09:15 - 03/03/04!
print: anObject
	anObject printOn: self! !

!SequenceableCollection Enumerating methods 12:58 - 04/20/04!
doWithIndex: twoArgBlock
      | index |
      index := 1.
      self do:[ :each |
            twoArgBlock value: each value: index.
            index := index + 1]! !

!SequenceableCollection Enumerating methods 12:59 - 03/09/04!
from: start to: stop doWithIndex: aBlock

	"Iteratively evaluate the two argument block, aBlock using
	 the elements from start to stop in the reciever and the
	 element index.

	 Fail if aBlock is not a two argument block."

	aBlock applyWithIndex: self from: start to: stop.! !

!ContextBlock Operations methods 10:41 - 03/23/04!
applyWithIndex: aCollection from: start to: end

	start to: end do: [:i | self value: (aCollection at: i) value: i].
	^aCollection! !

!StaticBlock Operations methods 10:41 - 03/23/04!
applyWithIndex: aCollection from: start to: end

	start to: end do: [:i | self value: (aCollection at: i) value: i].
	^aCollection! !

!Number class Init / Release methods 04:22 - 01/23/04!
newFromString: aString 
	^( aString includes: $. )
		ifTrue: [ Float newFromString: aString ]
		ifFalse: [ Integer newFromString: aString ]! !

!Number Operations methods 09:29 - 01/23/04!
degreesToRadians

	"Answer an instance of Float representing  the
	 conversion of receiver (in degrees) to radians. "

	^self * Float pi / 180.0! !

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

!Number Operations methods 04:22 - 01/23/04!
sign
	"
	Returns 1 if the receiver is greater than zero, -1 if it is less than zero,
	else zero.
	Return Value: 
		An Integer.
	"
	(self > 0) ifTrue: [^1].
	(self < 0) ifTrue: [^-1].
	^0! !

!WinPoint Operations methods 02:53 - 01/26/04!
rotatedBy: radians 
	" Need to round this point because WinPoint expects integers "

	| sinVal cosVal |
	sinVal := radians negated sin.
	cosVal := radians negated cos.
	^self class new x: (cosVal * ( self x ) + ( sinVal * ( self y ) )) rounded y: ( (cosVal * ( self y )) - ( sinVal * ( self x ) ) ) rounded! !

!IdentityDictionary Private methods 02:08 - 01/19/04!
at: index ifAbsentPutUsing: aBlock
	^self at: index ifAbsentPut: aBlock! !

!Interval Accessing methods 07:44 - 11/23/02!
species
	"
	Answers a class which can be used to store the receiver's 
	elements.
	Return Value:
		A class similar to the receiver's class.
	Remarks:
	The species is used for containing derived copies of the receiver. The default 
	implementation answers OrderedCollection.
	"
	^Array! !

!Float class publicMethods methods 12:24 - 01/22/04!
pi
	^3.14159265358979! !

!Float Operations methods 12:24 - 01/22/04!
reciprocal

	"Answer a type of Number which represents 1
	 divided by the receiver.

	 Fail if the receiver is zero."

	^ 1.0 / self! !

!Fraction Operations methods 12:24 - 01/22/04!
reciprocal

	"Answer a type of Number which represents 1
	 divided by the receiver.

	 Fail if the receiver is zero."


	^denominator / numerator! !

!Integer Operations methods 08:48 - 01/23/04!
bitAnd: anInteger
	^self & anInteger! !

!Integer Operations methods 12:24 - 01/22/04!
power: aNumber

	"Answer a type of Number where the result is
	 aNumber raised to the receiver."

	| result |

	self == 0
		ifTrue: [^1].	"optimized"
	result := 1.
	self abs timesRepeat: [result := result * aNumber].
	self < 0
		ifTrue: [^result reciprocal].
	^result! !

!Integer Operations methods 12:24 - 01/22/04!
reciprocal

	"Answer a type of Number which represents 1
	 divided by the receiver.

	 Fail if the receiver is zero."

	self == 0 ifTrue: [^1 // 0].
	(self == 1 or: [self == -1]) ifTrue: [^self].
	^Fraction basicNew numerator: 1 denominator: self! !

!WordArray class Allocating methods 09:55 - 01/23/04!
decodeFrom: start from: source startingAt: index into: dest

	| from to w stop |
	to := start.
	from := index.
	stop := dest size.
	" Map groups of 4 source bytes into groups of 3 destination bytes. "
	[to <= stop]
		whileTrue:
			[w := (((source at: from) asInteger bitAnd: 63) bitShift: 18) +
				(((source at: from + 1) asInteger bitAnd: 63) bitShift: 12) +
				(((source at: from + 2) asInteger bitAnd: 63) bitShift: 6) +
				((source at: from + 3) asInteger bitAnd: 63).
			from := from + 4.
			dest at: to put: (w bitShift: -16).
			to < stop ifTrue:
				[dest at: to + 1 put: ((w bitShift: -8) bitAnd: 255).
				to + 1 < stop ifTrue:
					[dest at: to + 2 put: (w bitAnd: 255)]].
			to := to + 3]! !

!WordArray class Allocating methods 09:55 - 01/23/04!
fromPrintableString: packedString
	"Decode the receiver from a ByteArray that was a printable String.
	See ByteArray>>asPrintableString for algorithm details. "

	"ByteArray halt; fromPrintableString: '^24FS @a' 
	"

	| size result last resultSize |
	size := packedString size.
	size = 0 ifTrue: [^self new].
	last := packedString last asInteger.
	resultSize := size // 4 * 3.
	last >= 96
		ifTrue:  "not a multiple of 3"
			[resultSize := resultSize - 3 + last - 96].
	result := self new: resultSize.
	self decodeFrom: 1 from: packedString startingAt: 1 into: result.
	^result! !

!WordArray Operations methods 09:55 - 01/23/04!
asPrintableString
	" Encode the receiver into a printable String.
	We divide up the receiver into 6-bit chunks,
	and turn each one into a character whose code
	is in the range [32..95] (The terminating character
	of the string is in the range [32..98]).   Note
	that the result size is always a multiple of 4."

	"ByteArray fromPrintableString: #[12 34 56 78 90 123] asPrintableString"

	| result |
	result := self class new: self size + 2 // 3 * 4.
	self encodeFrom: 1 into: result startingAt: 1.
	^String fromBytes: result.! !

!WordArray Operations methods 09:55 - 01/23/04!
compressedStoreOn: aStream 

	"Write a reduced storeString of the receiver using the #[] notation for small
	byteArray and a compressed string representation for larger ones"

	self size < 50
		ifTrue: 
			[aStream nextPutAll: '#[ '.
			self do: [ :each | 
				aStream print: each ; space].
			aStream nextPut: $]]
		ifFalse: 
			[aStream 
				nextPutAll: '(ByteArray fromPrintableString:' 
				; print: self asPrintableString 
				; nextPut: $)]! !

!WordArray Operations methods 09:55 - 01/23/04!
encodeFrom: start into: dest startingAt: index

	| from to w stop |
	from := start.
	to := index.
	stop := self size.
	" Map groups of 3 source bytes into groups of 4 destination bytes."
	[from <= stop]
		whileTrue:
			[w := (self at: from) bitShift: 16.
			from < stop ifTrue:
				[w := w + ((self at: from + 1) bitShift: 8).
				from + 1 < stop ifTrue:
					[w := w + (self at: from + 2)]].
			w := w bitXor: 16r820820.  "flip the high bit of each group of 8"
			from := from + 3.
			dest at: to put: (w bitShift: -18) + 32.
			dest at: to + 1 put: ((w bitShift: -12) bitAnd: 63) + 32.
			dest at: to + 2 put: ((w bitShift: -6) bitAnd: 63) + 32.
			dest at: to + 3 put: (w bitAnd: 63) + 32.
			to := to + 4].
	"If the size is not a multiple of 3 bytes, encode the remainder in the last character
	of the result, which is unused in this case. Use a code that is not in the range [32..95]."
	stop \\ 3 = 0 ifFalse:
		[dest at: to - 1 put: stop \\ 3 + 96]! !

!String class Allocating methods 10:31 - 01/26/04!
fromBytes: aByteArray
	| length instance |
	length := aByteArray size.
	instance := self new: length.
	1 to: (length) do: [ :pos |
		instance at: pos put: (aByteArray at: pos) ].
	^instance.! !

!String Converting methods 04:29 - 01/22/04!
readStream
	^ReadStream on: self! !

!String Converting methods 04:29 - 01/22/04!
writeStream
	^WriteStream on: self! !

!LargeInteger Operations methods 10:48 - 01/28/04!
arcTan
	^(self asFloat) arcTan! !

!SmallInteger Operations methods 10:48 - 01/28/04!
arcTan
	^(self asFloat) arcTan! !

Compiler addCatDescription: 'Allocating'A text: 'Allocates a new object.'A.
Compiler addCatDescription: 'Converting'A text: 'Converting
Convert the receiver to another type.'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: 'Init / Release'A text: 'Init / Release
Initialization and Release methods.'A.
Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Exception handling'A text: 'Exception handling
Exception Handling Interface.'A.
Compiler addCatDescription: 'Copying'A text: 'Copying
Answer a new object with properties of the receiver.'A.
Compiler addCatDescription: 'Private'A text: 'Private
Private methods.'A.
Compiler addCatDescription: 'Testing'A text: 'Testing
Test the receiver and answer a Boolean.'A.
Compiler addCatDescription: 'Enumerating'A text: 'Enumerating
Enumerate elements.'A.!
