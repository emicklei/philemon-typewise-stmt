""
PROJECTNAME "PhilemonTerminalClientStMt Preload" .
PROJECTCATEGORY "Philemon Terminal Client" .
PREREQUISITES internet .

CLASS TerminalClientHttpService
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS HTTPSession
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	openPostRequest:
END
! !
Object subclass: #TerminalClientHttpService
	instanceVariableNames: 'urlParts internet session request synchronizer'
	classVariableNames: 'netmodule secureIcon insecureIcon'
	poolDictionaries: 'WinInetConstants WinUserConstants'
	comment: ''
	category: ''!

!TerminalClientHttpService class * methods 03:57 - 03/12/04!
at: anUrlString 
	| instance |
	instance := self new initialize.
	instance url: anUrlString.
	^instance! !

!TerminalClientHttpService Private methods 12:22 - 03/15/04!
agentName
	^'TypeWise'! !

!TerminalClientHttpService Init / Release methods 04:00 - 03/12/04!
initialize! !

!TerminalClientHttpService Private methods 12:28 - 03/15/04!
internet
	internet isNil ifTrue: [ internet := Internet openForAgent: self agentName ].
	^internet! !

!TerminalClientHttpService Private methods 01:50 - 03/15/04!
session 
	^session! !

!TerminalClientHttpService Private methods 12:37 - 03/15/04!
session: newSession
	session isNil ifFalse: [ session close ].
	session := newSession.! !

!TerminalClientHttpService Init / Release methods 02:56 - 03/15/04!
shutDown
	session isNil ifFalse: [ session close. session := nil. ].
	internet isNil ifFalse: [ internet close. internet := nil ].! !

!TerminalClientHttpService Init / Release methods 04:00 - 03/12/04!
startUp! !

!TerminalClientHttpService * methods 01:53 - 03/15/04!
url: urlString
	urlParts := Internet crackUrl: urlString flags: 0.
	self session: (self internet connectHTTP: (urlParts at: #lpszHostName ifNone: 'localhost') 
							 port: (urlParts at: #nPort ifNone: INTERNET_SERVICE_HTTP)
							 flags: 0).! !

!HTTPSession HTTP methods 02:16 - 03/24/04!
openPostRequest: lpszObjectName
	"
	Creates an HTTP request handle which uses HTTP Post instead of GET.
	Parameters:
		lpszObjectName   Name of the target object of the specified verb. 
		                 This is generally a file name, an executable module, or a search specifier. 
		lpszVersion      HTTP version. If this parameter is NULL, the function uses HTTP/1.0 as the version. 
		lpszReferer      URL of the document from which the URL in the request (lpszObjectName) was obtained. 
		                 If this parameter is NULL, no `referrer` is specified. 
		lpszAcceptTypes  Media type strings accepted by the client, or NULL.
		dwFlags          Internet flag (INTERNET_FLAG_XXX constant).
	Return Value: 
		An HTTPRequest if successful, otherwise nil.
	"
	|lpszAcceptTypes hInternet|
	lpszAcceptTypes := WordArray localNew: 64.
	lpszAcceptTypes reset.
	MIMETable basicDo: [ :i :szType |
		lpszAcceptTypes _at: i put: szType
	].
	(hInternet := WINAPI HttpOpenRequest: self
		with: 'POST'
		with: lpszObjectName
		with: NULL
		with: NULL
		with: lpszAcceptTypes
		with: INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_RELOAD
		with: self basicAddress) ~~ NULL ifTrue: [
		^HTTPRequest value: hInternet
	].
	_DEBUG_USER == TRUE ifTrue: [
		MessageBox printLastError: (String wsprintf: '%s(0x%x) : HttpOpenRequest'
			_with: self _class name
			_with: self).				
	]. 	
	^nil! !

Compiler addCatDescription: 'HTTP'A text: 'HTTP'A.
Compiler addCatDescription: 'Init / Release'A text: 'Init / Release
Initialization and Release methods.'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Private'A text: 'Private
Private methods.'A.!
