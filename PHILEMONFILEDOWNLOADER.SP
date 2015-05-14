""
PROJECTNAME PhilemonFileDownloader .
PROJECTCATEGORY  .

CLASS ApplicationProcess
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	threadWithId:
END

CLASS FileDownloader
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS FileDownloadItem
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS Internet
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	openUrl:flags:
END
! !
Object subclass: #FileDownloader
	instanceVariableNames: ''
	classVariableNames: 'queue threads canDownload queueMutex threadsMutex internet killThreads '
	poolDictionaries: ''
	comment: ''
	category: ''!
Object subclass: #FileDownloadItem
	instanceVariableNames: 'requestor location state '
	classVariableNames: ''
	poolDictionaries: 'WinInetConstants '
	comment: ''
	category: ''!

!ApplicationProcess Accessing methods 11:17 - 06/08/04!
threadWithId: aThreadId
	" Get a thread using a thread ID "

	" aThreadId should only occour once. If multiple threads have the 
	  same ID, only the first is returned "
	self threads do: [ : each |
		each isNil ifFalse: [
			(each id = aThreadId) ifTrue: [ ^each ]
		]
	].
	^nil.! !

!FileDownloader class Private methods 10:31 - 10/04/04!
addDownloadToQueue: aFileDownloadItem
	" Warning: this is a private method. Use downloadFrom:notifyWhenReady: method "  
	
	Logger debug: [ '-- Adding to download queue: ', aFileDownloadItem asString ].
	self checkInitialized isNil ifTrue: [ ^nil ].
	queueMutex wait.
	queue add: aFileDownloadItem. 
	queueMutex release.
	canDownload release.
	" Yield "
	Processor sleep: 20.! !

!FileDownloader class * methods 11:26 - 06/16/04!
cancelDownloadWithNotifyObject: anObject
	| runningRequests |
	" anObject must the callback object "
		
	" Critical start "
	queue isNil ifFalse: [
		queueMutex wait.
		queue := queue reject: [ :each | each requestor = anObject ].
		queueMutex release.
		].
    
    threads isNil ifFalse: [ 
    	threadsMutex wait.
		runningRequests := threads valuesDo: [ :each | 
	  	each isNil ifFalse: [
			(each requestor = anObject) ifTrue: [ each abort ] ] ].
		threadsMutex release.
		].
	
	^true! !

!FileDownloader class Private methods 12:21 - 10/06/04!
checkInitialized
	|  procString |
	" Private method "
	
	" Check if any downloadthread exist. Check if queue exists.
	Set semaphore to be able to signal when a download should be started "

  	" This method presents a interesting way of chicken-egg. "
  	" Initializing mutexes should be protected by a critical section "
  	" But where to define this critical section? "
  
	" Start critical "
	"   -- it should be impossible for two threads to initialize at the same time "
	procString := (WINAPI GetCurrentProcessId) asString.
	
	
	killThreads isNil ifTrue: [ killThreads := false ].
	killThreads ifTrue: [ Logger warn: 'Cannot add download when threads are closing down '. ^nil ].
	canDownload isNil ifTrue: [ 
		canDownload := Semaphore createSemaphore: ('FileDownloadStart', procString) initialCount: 0 maxCount: 500 " Large ". ].
	queueMutex isNil ifTrue: [ 
		queueMutex := WinMutex createMutex: ('downloadQueueMutex', procString) initialState: FALSE ].
	threadsMutex isNil ifTrue: [ 
		threadsMutex := WinMutex createMutex: ('downloadThreadMutex', procString) initialState: FALSE ].

	threadsMutex wait.
	threads isNil ifTrue: 
	[ 
		self startDownloadThreads: self defaultDownloadThreads ].
	threadsMutex release.

	queueMutex wait.
	queue isNil ifTrue: [ queue := OrderedCollection new ].
	queueMutex release.
	 

	" End critical "! !

!FileDownloader class Properties methods 08:52 - 06/24/04!
defaultDownloadThreads
	^6! !

!FileDownloader class * methods 03:00 - 06/07/04!
downloadFrom: aString 
	" Useless method, but for testing " 
	" Do not use!! "
	self downloadFrom: aString notifyObject: nil.! !

!FileDownloader class * methods 03:01 - 06/07/04!
downloadFrom: aString notifyObject: anObject
	" anObject must understand some messages "
	" aString holds the url/filename "
	self addDownloadToQueue: (FileDownloadItem for: anObject location: aString)! !

!FileDownloader class Private methods 12:22 - 10/06/04!
downloadThread
	" private method "
	
	^[ " This is the code executed in the thread "
		" While suicide is not neccesary, try working " 
		
	 	[ self shouldCommitSuicide ]
	    whileFalse: 
		[ | obj | 
		  obj := self threadWait. 
		  obj isNil ifFalse: [ self threadWork: obj ] ifTrue: [ Logger debug: 'Thread received nil work ' ]. 
		]. 
	] forkAt: 0 "Normal priority "! !

!FileDownloader class Private methods 04:01 - 06/08/04!
internet
	internet isNil ifTrue: [
		internet := Internet openForAgent: 'FileFetcher'.
	].
	^internet! !

!FileDownloader class Private methods 12:46 - 06/08/04!
registerWorkingOn: aWorkObject
	" Private " 
	" register current thread as working on aWorkObject "
	threadsMutex wait.
	threads at: WINAPI GetCurrentThreadId put: aWorkObject.	
	threadsMutex release.! !

!FileDownloader class Properties methods 12:22 - 10/06/04!
release
	" 
	  FileDownloader release.
	  killThreads := false
    "

	" Release internet "
	internet isNil ifFalse: [ internet close. internet := nil. ].
	
	" Empty queue "
	queue := nil.
	
	" Let threads terminate "
	killThreads := true.
	
	" Be sure they check if they should commit suicide "
	(1 to: threads size + 1) do: [ : each | canDownload release ].
	
	" Start a thread to kill threads "
	[ Processor sleep: 100.
	  threads isNil ifFalse: [ 
	  	threads keysDo: [ : key | | thread | 
			thread := Processor threadWithId: key.
			thread isNil ifFalse: [
				Processor outputDebugLine: 'Killing thread ', thread id asString.
				Processor terminateThread: thread exitCode: 0.
				].
			].
		].
	  threads := nil.
	  canDownload := nil.
	  queueMutex := nil.
	  threadsMutex := nil.
	  killThreads := false ] fork.! !

!FileDownloader class Private methods 10:45 - 06/16/04!
shouldCommitSuicide
	" PRIVATE!! "
	" Never, ever call this method from a 'main thread', this
	     could generate undefined and unwanted behaviour. "
	

	^killThreads! !

!FileDownloader class Private methods 04:35 - 06/08/04!
startDownloadThreads: aNumber
	" Private "
 	
	threadsMutex wait.
	threads isNil ifTrue: [ threads := Dictionary new. ]. 
	threadsMutex release.
	(1 to: aNumber) do: [ :num | self downloadThread. ].! !

!FileDownloader class Private methods 11:24 - 06/08/04!
threadWait
	" Private "
	" Should only be called in a thread "
	
	" Return FileDownloadItem instance or nil "
	| workObject |
	
	self registerWorkingOn: nil.
	" Wait until we can download something "  
	canDownload wait.
	
	" Critical start "
	queueMutex wait.         
	(queue isNil or: [ queue isEmpty ]) 
		ifTrue: [ workObject := nil ] 
		ifFalse: [ workObject := queue removeFirst. ].
	queueMutex release.

	" Critical stop "
	^workObject! !

!FileDownloader class Private methods 10:31 - 10/04/04!
threadWork: workObject
	" Private "
	" Should only be called in a thread "
	" anObject is the object that needs to be worked "
	
	self registerWorkingOn: workObject.
	Logger debug: [ '-- Starting work for: ', workObject asString ].
	workObject doIt.
	self registerWorkingOn: nil.
	Logger debug: [ '-- Finished work for: ', workObject asString ].! !

!FileDownloadItem class * methods 12:51 - 06/07/04!
for: aRequestor location: aUrl
	^self new
		requestor: aRequestor
		; location: aUrl
		; state: nil
		; yourself.! !

!FileDownloadItem * methods 03:28 - 06/08/04!
abort
	self state: 'abort'.! !

!FileDownloadItem Operations methods 01:00 - 06/08/04!
dataFromFile: aFileName
	| cont file |
	file := (FileStream read: aFileName text: false).
	cont := file contents.
	file close.
	^cont.! !

!FileDownloadItem Operations methods 11:03 - 10/22/05!
doIt
	| result |
	result := (location beginsWith: 'http')
			ifTrue: [ self handleData: (self downloadFromUrl) ] 
			ifFalse: [ self handleFileName: location ].! !

!FileDownloadItem Operations methods 10:32 - 06/25/04!
downloadFromUrl
	| connection data read |
	" Load image FromIStream"
    
    " Open internet connection to url "
    self shouldAbort ifTrue: [ ^nil ].
	connection := FileDownloader internet openUrl: location flags: 0.
	connection isNil ifTrue: [ self requestor downloadError: 'Cannot connect: '. ^nil ].
	self shouldAbort ifTrue: [ connection close. ^nil ]. 
	data := WriteStream on: ByteArray new.
	" Read data from connection "
	[ 
		read := connection readData.
		self shouldAbort ifTrue: [ connection close. ^nil ]. 
		read isNil
	] whileFalse: [
		data nextPutAll: read asByteArray.
	].
	connection close.
	^data contents! !

!FileDownloadItem Operations methods 01:00 - 06/08/04!
fileForData: data
	| fileName |
	fileName := FileStream getTempFileName.
	(FileStream createFile: fileName) nextPutAll: data; close.
	^fileName! !

!FileDownloadItem Operations methods 03:33 - 06/08/04!
handleData: data
	self shouldAbort ifTrue: [ ^false ].
	data isNil ifTrue: [ ^false ].
	requestor acceptsData 
		ifTrue: [ requestor downloadedData: data ]
		ifFalse: [ requestor acceptsFileName 
			ifTrue: [ 
				| file |
			 	file := (self fileForData: data). 
			 	self shouldAbort ifTrue: [ ^false ].
				requestor downloadedFile: file ]
			ifFalse: [ ^false ] ].
	^true.! !

!FileDownloadItem Operations methods 03:32 - 06/08/04!
handleFileName: aFileName
	self shouldAbort ifTrue: [ ^false ].
	requestor acceptsFileName 
		ifTrue: [ requestor downloadedFile: aFileName ]
		ifFalse: [ requestor acceptsData 
			ifTrue: [ 
				| data |
				data := (self dataFromFile: aFileName).
				self shouldAbort ifTrue: [ ^false ]. 
				requestor downloadedData: data ]
			ifFalse: [ ^false ] ].
	^true.! !

!FileDownloadItem * methods 03:01 - 06/07/04!
location
	^location! !

!FileDownloadItem * methods 03:01 - 06/07/04!
location: aString
	location := aString! !

!FileDownloadItem Operations methods 12:33 - 10/01/04!
printOn: aStream
	aStream nextPutAll: 'FileDownloadItem(' ; 
			nextPutAll: location ; 
			nextPutAll: ', ';
			nextPutAll: (requestor asString) ; 
			nextPutAll: ', ';
			nextPutAll: (state asString) ; 
			nextPutAll: ')'.! !

!FileDownloadItem * methods 03:01 - 06/07/04!
requestor
	^requestor! !

!FileDownloadItem * methods 03:01 - 06/07/04!
requestor: anObject
	requestor := anObject! !

!FileDownloadItem * methods 03:35 - 06/08/04!
shouldAbort
	state isNil ifTrue: [ ^false ].
	^true! !

!FileDownloadItem * methods 03:01 - 06/07/04!
state
	^state! !

!FileDownloadItem * methods 03:01 - 06/07/04!
state: stateDescriptor
	state := stateDescriptor! !

!Internet File Handling methods 10:41 - 06/16/04!
openUrl: szUrl flags: flags
	"
	Begins reading a complete FTP, Gopher, or HTTP URL.
	Parameters:
		szUrl		A string containing the URL to begin reading. Only URLs beginning with 
				ftp:, gopher:, http:, or https: are supported.
	Return value:
		A handle to an internet file if successful, otherwise nil.
	" 
	|hInternet|
	(hInternet := WINAPI InternetOpenUrl: self
		with: szUrl
		with: NULL
		with: 0 
		with: flags 
		with: 0) ~~ NULL ifTrue: [
		^Internet value: hInternet
	].
	_DEBUG_USER == TRUE ifTrue: [
		MessageBox printLastError: (String wsprintf: '%s(0x%x) : InternetOpenUrl(%s)'
			_with: self _class name
			_with: self
			_with: szUrl).				
	]. 	
	^nil! !

Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: 'File Handling'A text: 'File Handling
Document handling methods.'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: 'Private'A text: 'Private
Private methods.'A.
Compiler addCatDescription: 'Properties'A text: 'Properties
Protocol that relates to properties defined within the receiver.'A.!
