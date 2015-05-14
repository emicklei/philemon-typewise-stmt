""
PROJECTNAME PhilemonStMtInternetExtensions .
PROJECTCATEGORY  .

CLASS SstUrlAdapter
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS String
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	sstAsUrl
END

CLASS HTTPRequest
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	queryStatusCode
END
! !
Object subclass: #SstUrlAdapter
	instanceVariableNames: 'urlParts '
	classVariableNames: 'lastUrlCrackError '
	poolDictionaries: ''
	comment: 'This class understands (a subset of) the VASt SstUrl interface. Can be used for porting purposes.'
	category: ''!

!SstUrlAdapter class * methods 12:58 - 08/20/04!
fromString: anUrlString
	" Internet crackUrl: '' flags: 0.
	  Internet crackUrl: 'http://www.av.com' flags: 0.   
	  	--> MappingTable(#lpszScheme>>'http' #lpszHostName>>'www.av.com' #nPort>>80 )
	  Internet crackUrl: 'http://www.url.com/path/to/file/thefile?key=value' flags: 0.
	    --> MappingTable(#lpszScheme>>'http' 
	    				 #lpszHostName>>'www.url.com' 
	    				 #nPort>>80 
	    				 #lpszUrlPath>>'/path/to/file/thefile' 
	    				 #lpszExtraInfo>>'?key=value\0U\0\0\0\0\0\0\0' ) "       
	| crackedUrlParts instance |
	crackedUrlParts := Internet crackUrl: anUrlString flags: 0.
	crackedUrlParts isNil ifTrue: [
		lastUrlCrackError := MessageBox formatLastError.
	 	^nil 
	].
	instance := self new.
	instance urlParts: crackedUrlParts.
	^instance! !

!SstUrlAdapter class * methods 12:52 - 08/20/04!
lastError
	^lastUrlCrackError ifNil: [ 'No last error' ].! !

!SstUrlAdapter Interface methods 12:49 - 08/20/04!
address
	^(urlParts at: #lpszHostName ),':',(urlParts at: #nPort) asString! !

!SstUrlAdapter Interface methods 12:44 - 08/20/04!
path
	^urlParts at: #lpszUrlPath! !

!SstUrlAdapter Interface methods 12:44 - 08/20/04!
transport
	^urlParts at: #lpszScheme! !

!SstUrlAdapter Interface methods 12:44 - 08/20/04!
urlParts: aUrlStruct
	urlParts := aUrlStruct! !

!String Converting methods 12:48 - 08/20/04!
sstAsUrl
	" Returns a SstUrlAdapter instance or nil if unsuccesfull "	
	^SstUrlAdapter fromString: self! !

!HTTPRequest Operations methods 08:34 - 01/18/05!
queryStatusCode        
	|buffer bufferSize|     
         
	bufferSize := LONG value: 4.    
	buffer := LONG new.
		WINAPI HttpQueryInfo: self
			with: HTTP_QUERY_FLAG_NUMBER | HTTP_QUERY_STATUS_CODE
			with: buffer basicAddress
			with: bufferSize basicAddress
			with: LONG localNew.
		^buffer
	"^self queryInfo: HTTP_QUERY_FLAG_NUMBER | HTTP_QUERY_STATUS_CODE"! !

Compiler addCatDescription: 'Interface'A text: 'Interface'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Converting'A text: 'Converting
Convert the receiver to another type.'A.!
