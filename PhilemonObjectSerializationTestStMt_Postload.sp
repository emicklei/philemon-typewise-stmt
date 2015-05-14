""
PROJECTNAME "PhilemonObjectSerializationTestStMt Postload" .
PROJECTCATEGORY "Philemon Object Serialization" .

PROFILE
BEGIN
END

CLASS BinaryObjectAccessorTest
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	testAll
	testStoreByReference
END

! !

!BinaryObjectAccessorTest publicMethods methods 08:57 - 03/01/04!
testAll
	"self new testAll"

	| d copy | 
	(d := Dictionary new: 100)
		at: 'point' put: 1@$a;
		at: 'behavior' put: Class;
		at: 'bytearray' put: (ByteArray with: 1 with: 2 with: 3 with: 4);
		at: 'string' put: (String fromBytes: (0 to: 255) asByteArray );
		"at: 'twoByteString' put: (DBString abtFromBytes: (0 to: 255) asByteArray );"
		at: 'boolean' put: true;
		at: 'character' put: $$;
		at: 'date' put: Date today;
		at: 'float' put: 0.0;
		at: 'fraction' put: 1 / 2;
		at: 'integer' put: 123456789;
		" at: 'message' put: (Message new receiver: nil selector: #halt); Message does not work in StMt !! " 
		at: 'ordered' put: (OrderedCollection with: 1 with: 'abc' with: nil);
		at: 'sorted' put: (SortedCollection with: 7 with: 3 with: 99);
		at: 'array' put: #(1 2 3 4);
		at: 'set'  put: (Set with: Object);
		at: 'symbol' put: #help;
		at: 'time' put: Time now;
		at: 'nil' put: #(nil).
	copy := BinaryObjectAccessor objectFromByteArray: (BinaryObjectAccessor byteArrayFromObject: d).
	copy keysAndValuesDo: [ :k :v |	
		self assert: (d at: k ifAbsent: []) class = v class]! !

!BinaryObjectAccessorTest publicMethods methods 08:54 - 03/01/04!
testStoreByReference

" 	TODO: Make this test succeed by implementing TObject a good way. 
   		This test does not work in StMt "! !
