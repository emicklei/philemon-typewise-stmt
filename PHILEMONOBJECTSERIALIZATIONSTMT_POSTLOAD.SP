""
PROJECTNAME "PhilemonObjectSerializationStMt Postload" .
PROJECTCATEGORY "Philemon Object Serialization" .


PROFILE
BEGIN
END

CLASS BinaryObjectAccessor
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	initializeReadSelectors
END
INSTANCEMETHODS 
BEGIN
	storeSymbol:
	writeFloat:
	readFloat
	readHeaderFrom:
END
! !

!BinaryObjectAccessor class publicMethods methods 04:10 - 02/23/04!
initializeReadSelectors

| inStream |                                        
inStream := ReadStream on: self readSelectorEntries.
[inStream atEnd]
whileFalse:
          [ | key value |
          key := inStream next asString. 
          value := inStream next. 
          self readSelectors at: ( Compiler poolValueAt: key) put: value ]! !

!BinaryObjectAccessor publicMethods methods 04:27 - 02/26/04!
readFloat

	"Read VA representation of Float"

	| aFloat |
	aFloat := Float basicNew: stream next.
	aFloat basicSize 
		to: 1 
		by: -1 
		do: [ :i | aFloat _byteAt: i put: stream next ].
	^aFloat! !

!BinaryObjectAccessor publicMethods methods 12:43 - 03/22/04!
readHeaderFrom: inStream

	| itsVersion start |
	start := ( inStream next: 3) asString .
	(start = 'BOA'
		and:[  inStream next <= version ])
			ifFalse:[self error: 'incompatible BOA format: ', start]! !

!BinaryObjectAccessor publicMethods methods 01:48 - 03/29/04!
storeSymbol: aSymbol
	"Pre: aSymbol can be represented by a sequence of bytes"
    " A Symbol is no string in StMt, convert it "
	self storeByteString: aSymbol asString! !

!BinaryObjectAccessor publicMethods methods 04:27 - 02/26/04!
writeFloat: aFloat 

	stream nextPut: TFloat.
	stream nextPut: aFloat basicSize.
	aFloat basicSize 
		to: 1 
		by: -1 
		do: [ :i | | bytePart |
		 bytePart := ( aFloat _byteAt: i ).
		 stream nextPut: bytePart ]! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
