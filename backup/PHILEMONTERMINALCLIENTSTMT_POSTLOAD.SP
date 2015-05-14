"TerminalClientStMt Postload"
PROJECTNAME "PhilemonTerminalClientStMt Postload" .
PROJECTCATEGORY "Philemon Terminal Client" .


PROFILE
BEGIN
  BUILD.APPNAME = ''
  BUILD.INCLUDESYMBOLTABLE = 1
  BUILD.OPTIMIZESTRUCTS = 0
  BUILD.RES = 1
  BUILD.CLEARGLOBALS = 0
  BUILD.DEBUGMODE = 0
  BUILD.EXCLUDESUPERMETHODS = 0
  BUILD.SPLITDEBUGSYMBOLS = 0
END

CLASS BinaryObjectAccessor
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	readHeaderFrom:
END

CLASS TerminalClient
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	runtimeInitialize
END
INSTANCEMETHODS 
BEGIN
	terminal
	indexedRequest:withArguments:
	isRuntime
	startProgramWith:
	showErrorString:
	openURL:
	handleByteArray:
	handleHtmlReply:
END

CLASS TerminalClientHttpService
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	netmodule:
	extendedErrorMessages
END
INSTANCEMETHODS 
BEGIN
	request:withArguments:
	indexedRequest:withArguments:
	handleNetworkError
	basicPost:to:
	urlPath
	urlParts
	urlParts:
	url:
	agentName
	handleNoDialogNetworkError:
	handleDialogNetworkError:subject:
	handleNetworkError:subject:
	handleNetworkErrorForSubject:
	doWithErrorHandling:subject:
	isDialogError:
	shutDown
	postMessage:
END

CLASS HTTPSession
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	openPostRequest:
	openPostRequest:flags:
END

CLASS TerminalTimer
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	show
	stopThread
	aboutToRemoveFromTerminalForm:
	isActive
END
! !

!BinaryObjectAccessor publicMethods methods 12:43 - 03/22/04!
readHeaderFrom: inStream

	| itsVersion start |
	start := ( inStream next: 3) asString .
	(start = 'BOA'
		and:[  inStream next <= version ])
			ifFalse:[self error: 'incompatible BOA format: ', start]! !

!TerminalClient class publicMethods methods 11:48 - 06/07/04!
runtimeInitialize
	" VERY, VERY, VERY, VERY ugly method "
	" You better forget about this method. "
	" This method references symbols that are not referenced elsewhere to make
	  sure the methods will turn up in the eventual image.. We'll fool the 
	  compiler by suggesting we call these symbols here. In fact, we 
	  return ASAP. "
	
	" Any human will read this as a 'return always', but the compiler will 
	    consider the possibility that nil is not nil today "  
	nil isNil ifTrue: [ ^self ].
	
	" It is extremely important to make sure the IP will never reach this point "
	" If it does, it will surely crash the program "
	TerminalWidgetBinaryAccessor setupAccessors. " This includes the TWBA symbol tree "
	nil evaluateTerminalMessage: nil with: nil.
	nil value: nil value: nil.
	nil submitInputNamesAndValuesTo: nil.
	nil value: nil value: nil.
	GdiplusBase startUp: nil. 
	GdiplusBase shutDown: nil.! !

!TerminalClient publicMethods methods 10:52 - 06/30/04!
handleByteArray: bin 
 	| replyFile firstBytes |
	(bin isNil or: [ bin isEmpty ])
		ifTrue: [ ^nil ].
	" Dump to file "
	replyFile := (FileStream createFile: 'lastreply.bin').
	replyFile isNil ifFalse: [ replyFile nextPutAll: bin; close. ] ifTrue: [ Processor outputDebugLine: '-- Could not open reply file' ].
	(bin size > 3) ifTrue: [
	    firstBytes := bin asString trimBlanks.
	    (firstBytes size > 3) ifTrue: [ 
	    	firstBytes := firstBytes copyFrom: 1 to: 4.
			((firstBytes beginsWith: '<ht') or: [ firstBytes beginsWith: '<HT' ]) ifTrue: [
				" Case HTML "
				self handleHtmlReply: bin.
				^nil
			].
			(firstBytes beginsWith: '<!!') ifTrue: [
				" Case XHTML "
				self handleHtmlReply: bin.
				^nil
			].                                                   
		].
		^TerminalWidgetBinaryAccessor objectFromByteArray: bin
	] ifFalse: [ Processor outputDebugLine: '-- Reply from server is too short; ', bin size asString, ' bytes' ].! !

!TerminalClient publicMethods methods 01:23 - 06/22/04!
handleHtmlReply: bin
	| fileName |
	fileName := (FileStream getTempFileName), '.html'.
	(FileStream createFile: fileName) nextPutAll: bin; close.
	self openURL: fileName.
	^nil! !

!TerminalClient publicMethods methods 02:41 - 05/21/04!
indexedRequest: index withArguments: anArray
	"Send the operation to the application"

	| reply  message|
	self terminal notify: 'Waiting...'.
	reply := self application indexedRequest: index withArguments: anArray.
	self terminal notify: ''.
	reply isNil
		ifTrue: [ ^nil ].
	reply isByteArray 
		ifFalse: [ 
			MessageBox printLastError: reply.
			^nil.
		].
	message := self handleByteArray: reply.
	^message isNil
		ifTrue: [ nil ]
		ifFalse:[ self safelySendMessage: message]! !

!TerminalClient publicMethods methods 01:43 - 06/01/04!
isRuntime
	^(_IS_DEVELOPMENT == TRUE) not! !

!TerminalClient publicMethods methods 11:21 - 06/11/04!
openURL: aUrlString
	self startProgramWith: aUrlString! !

!TerminalClient publicMethods methods 02:49 - 06/02/04!
showErrorString: aString
	| win txt winSize close |
	winSize := terminal screen isNil
		ifTrue: [ 20 @ 40 ]
		ifFalse: [ terminal screen bounds extent ].
	win := TerminalWidget windowIn: ( 1 @ 1 extent: winSize ).
	win background: Color blue.
	win foreground: Color yellow. 
	
	txt := TerminalWidget textIn: ( 1 @ 1 extent: ( winSize x - 1 @ winSize y ) ).
	txt editable: false.
	txt string: 'Error:' , aString. 
	
	close := TerminalWidget buttonIn: ( winSize x + 1 @ 2 extent: 0 @ ( winSize y - 2 ) ).
	close label: 'Runtime Error Occurred: Press Enter To Exit'.
	close when: TerminalWidgetEvent clicked send: #close to: self. 
	
	win add: close.
	win add: txt.
	self show: win! !

!TerminalClient publicMethods methods 10:43 - 06/01/04!
startProgramWith: aCommandString
	" This could be hacker's dream "
	" Execute a command (possibly) specified from a remote location "
	
	WinProcess winExec: 'rundll32.exe url.dll,FileProtocolHandler ', aCommandString show: true.! !

!TerminalClient publicMethods methods 03:48 - 03/16/04!
terminal  
	" The main message loop controller requires the initialize routine
	to return the main window, that's where this method comes in. "
	^terminal! !

!TerminalClientHttpService class * methods 02:48 - 05/24/04!
extendedErrorMessages
	^Dictionary new 
		  at: 12006  put: 'This error can be caused by incorrect command line parameters'
  		; at: 12007  put: 'This error can be caused because the server name was spelled incorrectly'
		; at: 12029 put:  'Check your internet connection and server url'
		; yourself! !

!TerminalClientHttpService class * methods 02:08 - 05/24/04!
netmodule: aModule
	netmodule := aModule! !

!TerminalClientHttpService Private methods 01:59 - 05/24/04!
agentName
	^'TypeWise'! !

!TerminalClientHttpService Private methods 03:38 - 05/28/04!
basicPost: binaryData to: path
	" Post a request to the specified path. If this fails for some reason,
	  call handleNetworkError, and return it's value. (usually nil). 
	  
	  When succesfull, a bytearray is returned, containing the request data "
	|  data result flags |             
	                                                    
	" No session? no data. Not really an error though. "
	self session isNil ifTrue: [ ^nil ].
	
	" Lazy initialize, make sure we use the old session because we won't be bothered by the SSL dialog every time "
	request isNil 
		ifTrue: [
			flags := INTERNET_FLAG_NO_CACHE_WRITE 
				   | INTERNET_FLAG_RELOAD
				   | INTERNET_FLAG_PRAGMA_NOCACHE.
			(urlParts at: #lpszScheme ifNone: 'http') = 'https' ifTrue: [
				flags := flags 
						| SECURITY_FLAG_IGNORE_CERT_CN_INVALID
				 		| INTERNET_FLAG_SECURE ].
 
			Processor outputDebugLine: 'Creating post request '.
			request := self session openPostRequest: path flags: flags ].
	
	" Still no request? Then something is wrong "
	request isNil ifTrue: [ ^self handleNetworkErrorForSubject: nil ].

	Processor outputDebugLine: '-- Doing HTTP request '.
	(self doWithErrorHandling: [ request sendRequest: ('Content-length: ',binaryData sizeInBytes asString, '\0') optional: binaryData ] subject: request) isNil ifTrue: [ ^nil ].
	result := ByteArray new.
	[ 
		data := request readData.
		data isNil not 
	] whileTrue: [
		result := result , data
	].
	result isNil ifTrue: [ ^self handleNetworkErrorForSubject: nil ].
	Processor outputDebugLine:  '-- HTTP request succes, returning '.
	^result! !

!TerminalClientHttpService Operations methods 09:59 - 05/28/04!
doWithErrorHandling: aBlock subject: anInternetHandle
	" A block should evaluate to false if an error has occoured "
	
	" Returns to nil if an error has occoured, returns true if all is well "
	[ aBlock value = false ] whileTrue: [ " Handle exception " 
		| handleResult |
		handleResult := self handleNetworkErrorForSubject: anInternetHandle.
		handleResult isNil ifTrue: [ "Give up, nothing to do about this " ^nil ].
		(handleResult = ERROR_CANCELLED) ifTrue: [ " User cancelled operation " ^nil ].
	].
	^true! !

!TerminalClientHttpService Private methods 08:29 - 07/19/04!
handleDialogNetworkError: errorID subject: aInternetHandle
	| userChoice |
	" Handle dialog error here "  
	userChoice := ERROR_SUCCESS.
	userChoice := (aInternetHandle errorDlg: WINAPI GetDesktopWindow
			 error: errorID 
			 flags: FLAGS_ERROR_UI_FILTER_FOR_ERRORS 
			 		| FLAGS_ERROR_UI_FLAGS_GENERATE_DATA 
			 		| FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS).
				 		
	(userChoice = ERROR_INTERNET_FORCE_RETRY)
		ifTrue: [ ^ERROR_INTERNET_FORCE_RETRY ].
	(userChoice = ERROR_SUCCESS)
		ifTrue: [ ^ERROR_SUCCESS " Unless authentication... IMPLEMENT THIS " ].
	(userChoice = ERROR_CANCELLED)
		ifTrue: [ ^nil " Cancel -> nothing to do about this.. Unless authentication " ].
	(userChoice = ERROR_INVALID_HANDLE)	
		ifTrue: [ Processor outputDebugLine: '-- Error: dialog could not be opened because of invalid handle '. ^nil ].
	Processor outputDebugLine: '-- Error: internetErrorDlg returned unknown error response'.
	^nil.! !

!TerminalClientHttpService Operations methods 11:23 - 05/27/04!
handleNetworkError
	" Do not use unless absolutely sure no interneterrordlg is launced "
	| errorID |
	errorID := WINAPI GetLastError.
	^self handleNetworkError: errorID subject: nil.! !

!TerminalClientHttpService Private methods 08:37 - 07/19/04!
handleNetworkError: errorID subject: anInternetHandle
	" handle any network error the client has encountered "

	" Some errors can be resolved by using the system's standard 
	  dialogs (which can provide the user with specific information
	  and sometimes provide a choice (use certificate, ignore security 
	  warning, etc. "	

	" return 
		- nil for unrecoverable error
		- ERROR_CANCEL if user chooses to cancel the operation
		- ERROR_SUCCESS if user chooses to accept (should try again here). 
		- ERROR_INTERNET_FORCE_RETRY if the network operation should be tried again "
		
	"Construction with report error is needed because some errors need to pass an 
		alternative error to INTERNETCLIENTDLG. See http://support.microsoft.com/?kbid=195650
		for an example "
	| reportError |	
	reportError := (self isDialogError: errorID).
	^(reportError = false) 
		ifTrue:  [ self handleNoDialogNetworkError: errorID ]
		ifFalse: [ self handleDialogNetworkError: reportError subject: anInternetHandle ].! !

!TerminalClientHttpService Operations methods 11:23 - 05/27/04!
handleNetworkErrorForSubject: anInternetHandle
	" At least use this one " 
	| errorID |
	errorID := WINAPI GetLastError.
	^self handleNetworkError: errorID subject: anInternetHandle.! !

!TerminalClientHttpService Private methods 11:04 - 05/27/04!
handleNoDialogNetworkError: errorID
	" handle any network error the client has encountered that could not be handled by
	  the interneterrordialog defined by windows. This are typically the 'could not 
	  connect to server, click OK' errors, from which recovery (for now) is impossible.. "
	" try to feed the user with as much relevant information as possible "
	" return nil "
	| errorOSMessage errorMessageToUser extendedMessage |
	errorOSMessage := netmodule isNil 
		ifFalse: [ netmodule formatMsg: errorID ]
		ifTrue: [ MessageBox formatError: errorID ].
	errorOSMessage := errorOSMessage, (Internet getLastResponseInfo ifNil: [ '' ]) asString.
	errorMessageToUser := errorOSMessage, '\nWhile opening with the following parameters:',
		'\n  Host:\t', (self urlParts at: #lpszHostName ifNone: '<unknown>') asString,
		'\n  Port:\t', (self urlParts at: #nPort ifNone: '<unknown>') asString,
		'\n  Url:\t',  (self urlParts at: #lpszUrlPath ifNone: '<unknown>') asString.
	extendedMessage := self class extendedErrorMessages at: errorID asInteger ifNone: nil.
	extendedMessage isNil ifFalse: [ errorMessageToUser := (errorMessageToUser, '\n\n', extendedMessage) ].
	
	Processor outputDebugLine: '-- Network error: ', errorID asString, ': ', errorOSMessage.
	MessageBox owner: NULL
			text:  errorMessageToUser
			title: 'TypeWise - Error'
			style: MB_ICONEXCLAMATION|MB_SETFOREGROUND|MB_OK.
	
	^nil! !

!TerminalClientHttpService * methods 02:20 - 08/19/04!
indexedRequest: operationName withArguments: stringArguments
	" Todo: synchronise threads "
	" Present in post load because this method makes use of TBWA" 
	| message|
	message := IndexedMessageSend receiver: '_server' index: operationName arguments: stringArguments.
	^self postMessage: message.! !

!TerminalClientHttpService Private methods 08:33 - 07/19/04!
isDialogError: anErrorID
	(anErrorID = ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR) ifTrue: [ ^ERROR_INTERNET_HTTP_TO_HTTPS_ON_REDIR ].
	(anErrorID = ERROR_INTERNET_INCORRECT_PASSWORD) ifTrue: [ ^ERROR_INTERNET_INCORRECT_PASSWORD ].
	(anErrorID = ERROR_INTERNET_INVALID_CA) ifTrue: [ ^ERROR_INTERNET_INVALID_CA ].
	(anErrorID = ERROR_INTERNET_POST_IS_NON_SECURE) ifTrue: [ ^ERROR_INTERNET_POST_IS_NON_SECURE ].
	(anErrorID = ERROR_INTERNET_SEC_CERT_CN_INVALID) ifTrue: [ ^ERROR_INTERNET_SEC_CERT_CN_INVALID ].
	(anErrorID = ERROR_INTERNET_SEC_CERT_DATE_INVALID) ifTrue: [ ^ERROR_INTERNET_SEC_CERT_DATE_INVALID ].   
	(anErrorID = HTTP_STATUS_PROXY_AUTH_REQ) ifTrue: [ ^ERROR_INTERNET_INCORRECT_PASSWORD ].
	^false! !

!TerminalClientHttpService * methods 02:34 - 08/19/04!
postMessage: message          
	" Todo: synchronise threads "
	" Present in post load because this method makes use of TBWA" 
	| requestData replyData |
	requestData := TerminalWidgetBinaryAccessor byteArrayFromObject: message.
	replyData := self basicPost: requestData to: self urlPath.
	(replyData isNil || replyData isEmpty) 
		ifTrue: [ ^nil ]
		ifFalse: [ replyData isEmpty  
			ifTrue: [^'' ]
			ifFalse:[^replyData] ].! !

!TerminalClientHttpService * methods 02:21 - 08/19/04!
request: operationName withArguments: stringArguments

	| message  |
	message := MessageSend receiver: '_server' selector: operationName arguments: stringArguments.
	^self postMessage: message.! !

!TerminalClientHttpService Init / Release methods 09:39 - 05/28/04!
shutDown
	session isNil ifFalse: [ session close. session := nil. ].
	internet isNil ifFalse: [ internet close. internet := nil ].
	request isNil ifFalse: [ request close. request := nil. ].! !

!TerminalClientHttpService * methods 11:34 - 05/27/04!
url: urlString
	urlParts := Internet crackUrl: urlString flags: 0.
	urlParts isNil ifTrue: [ self handleNetworkErrorForSubject: nil. ^nil ].
	self session: (self internet connectHTTP: (urlParts at: #lpszHostName ifNone: 'localhost') 
							 port: (urlParts at: #nPort ifNone: INTERNET_SERVICE_HTTP)
							 flags: 0).! !

!TerminalClientHttpService Accessing methods 01:59 - 05/24/04!
urlParts  
	urlParts isNil ifTrue: [ urlParts := MappingTable new ].
	^urlParts! !

!TerminalClientHttpService Accessing methods 01:06 - 05/24/04!
urlParts: parts
	urlParts := parts! !

!TerminalClientHttpService Accessing methods 01:07 - 05/24/04!
urlPath
	^urlParts at: #lpszUrlPath ifNone: '/'! !

!HTTPSession HTTP methods 12:42 - 05/25/04!
openPostRequest: lpszObjectName
	"
	Creates an HTTP request handle which uses HTTP Post instead of GET.
	Parameters:
		lpszObjectName   Name of the target object of the specified verb. 
		                 This is generally a file name, an executable module, or a search specifier. 
		dwFlags          Internet flag (INTERNET_FLAG_XXX constant).
	Return Value: 
		An HTTPRequest if successful, otherwise nil.
	"
	^self openPostRequest: lpszObjectName 
		  flags: INTERNET_FLAG_NO_CACHE_WRITE | INTERNET_FLAG_RELOAD | INTERNET_FLAG_NO_UI | INTERNET_FLAG_PRAGMA_NOCACHE.! !

!HTTPSession HTTP methods 12:43 - 05/25/04!
openPostRequest: lpszObjectName flags: dwFlags
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
		with: dwFlags
		with: self basicAddress) ~~ NULL ifTrue: [
		^HTTPRequest value: hInternet
	].
	_DEBUG_USER == TRUE ifTrue: [
		MessageBox printLastError: (String wsprintf: '%s(0x%x) : HttpOpenRequest'
			_with: self _class name
			_with: self).				
	]. 	
	^nil! !

!TerminalTimer publicMethods methods 09:17 - 09/01/04!
aboutToRemoveFromTerminalForm: aForm
	super aboutToRemoveFromTerminalForm: aForm
	" self stopThread. "! !

!TerminalTimer publicMethods methods 09:27 - 09/03/04!
isActive
	^self form isNil not and: [ self visible ].! !

!TerminalTimer publicMethods methods 09:28 - 09/03/04!
show
	super show. 
	
	countThread := [ [ interval do: [ :where | self form notNil 
		ifTrue: 
			[ self string: where printString, ' '.
			self isActive ifTrue: [ ( Delay forSeconds: self delayInSeconds ) wait ].
			current := where.
			self isActive ifTrue: [ self triggerEvent: TerminalWidgetEvent counterCount ] ] ].
	self isActive ifTrue: [ self triggerEvent: TerminalWidgetEvent counterEnd ].
	self isActive                  
		and: [ self repeat ] ]
		whileTrue: [  ]. Processor outputDebugLine: 'Quitting timer thread'. ] fork! !

!TerminalTimer publicMethods methods 09:16 - 09/01/04!
stopThread
	" do not use this method unless absolutely sure what you're doing. "
	"   timer will kill itself when form is nil "
	| thread |                 
	thread := countThread.
	countThread := nil. " Prevent other threads to close thread "
	thread isNil ifFalse: 
		[ Processor terminateThread: thread exitCode: 0. ].! !

Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'HTTP'A text: 'HTTP'A.
Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: 'Init / Release'A text: 'Init / Release
Initialization and Release methods.'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Private'A text: 'Private
Private methods.'A.!
