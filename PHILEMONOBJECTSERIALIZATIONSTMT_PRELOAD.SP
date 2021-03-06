""
PROJECTNAME "PhilemonObjectSerializationStMt Preload" .
PROJECTCATEGORY "Philemon Object Serialization" .
POOLS SerializationConstants .


PROFILE
BEGIN
END

CLASS Stream
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	lineDelimiter
END

CLASS Signal
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS ByteArray
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	asString
END

CLASS Dictionary
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	phiWriteWith:
END

CLASS Block
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS KeyedCollection
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS DBString
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *
! !

Compiler poolAdd: 'SerializationConstants' value: (StringDictionary new: 43)!

(Compiler getPoolDictionary: 'SerializationConstants' asStringA)
	at: 'TInt'A put: 6;
	at: 'TFalse'A put: 3;
	at: 'TAck'A put: 254;
	at: 'TExpression'A put: 22;
	at: 'TOrderedCollection'A put: 27;
	at: 'TException'A put: 38;
	at: 'TRectangle'A put: 37;
	at: 'TFraction'A put: 29;
	at: 'TDouble'A put: 9;
	at: 'TDate'A put: 19;
	at: 'TChar'A put: 10;
	at: 'TTrue'A put: 2;
	at: 'TTwoByteString'A put: 33;
	at: 'MaxFloat'A put: 3402823000000000 �� @     #   �� ;.0;
	at: 'TDirectedMessage'A put: 21;
	at: 'TNamedForwarder'A put: 24;
	at: 'TFloat'A put: 8;
	at: 'TKeyedCollection'A put: 28;
	at: 'TArray'A put: 14;
	at: 'TZero'A put: 39;
	at: 'TObjectByReference'A put: 36;
	at: 'TMessage'A put: 20;
	at: 'TTime'A put: 18;
	at: 'TByteArray'A put: 31;
	at: 'TOwnerReference'A put: 23;
	at: 'TColor'A put: 99;
	at: 'MinFloat'A put: 0.00000000000000000000000000000000000001157494000000000;
	at: 'TBlock'A put: 26;
	at: 'TSet'A put: 15;
	at: 'TBehavior'A put: 16;
	at: 'TLong'A put: 7;
	at: 'TShort'A put: 5;
	at: 'TByte'A put: 4;
	at: 'TUnnamedForwarder'A put: 25;
	at: 'TDictionary'A put: 32;
	at: 'TObject'A put: 17;
	at: 'TSymbol'A put: 13;
	at: 'TNull'A put: 1;
	at: 'TSortedCollection'A put: 34;
	at: 'TUTF8String'A put: 12;
	at: 'TNotAck'A put: 255;
	at: 'TPoint'A put: 35;
	at: 'TByteString'A put: 11; yourself !
WinException subclass: #Signal
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
ContextBlock subclass: #Block
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
Dictionary subclass: #KeyedCollection
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
String variableByteSubclass: #DBString
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!

!Stream Operations methods 07:27 - 04/02/04!
lineDelimiter
	^String with: Character cr! !

!ByteArray Converting methods 02:25 - 02/25/04!
asString
	"Answer a copy of the receiver as a String."

	| siz |
    siz := self size.
	^(String new: siz)
		replaceFrom: 1 to: siz with: self startingAt: 1! !

!Dictionary publicMethods methods 02:16 - 02/02/04!
phiWriteWith: aWriter
	aWriter writeKeyedCollection: self.! !

!DBString class * methods 02:16 - 02/02/04!
abtFromBytes: bytes
	^self fromBytes: bytes! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Converting'A text: 'Converting
Convert the receiver to another type.'A.!
