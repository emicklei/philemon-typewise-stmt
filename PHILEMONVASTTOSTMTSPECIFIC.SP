""
PROJECTNAME PhilemonVAStToStMtSpecific .
PROJECTCATEGORY "Philemon StMt Specific code" .

CLASS AbtGraphicalObject
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS AbtAppBldrView
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *
! !
Object subclass: #AbtGraphicalObject
	instanceVariableNames: 'visible parent '
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!
FrameWindow subclass: #AbtAppBldrView
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!

!AbtGraphicalObject class * methods 10:05 - 03/30/04!
loadImageFromFile: szFile
	" Load image "
	" From file for now, but will add from url in the near future (another method probably)"
	| bmp |
	bmp := GdiplusBitmap new createFromFile: szFile useICM: false.
	(bmp getType = 0 " ImageTypeUnknown ") ifTrue: [ bmp delete. ^nil ].
	^bmp.! !

!AbtGraphicalObject class * methods 12:28 - 05/05/04!
loadImageFromUrl: szUrl
	| inet connection data read bmp |
	" Load image FromIStream"
    
    " Open internet connection to url "
	inet := Internet openForAgent: 'ImageFetcher'.
	connection := inet openUrl: szUrl. 
	data := IStream createStreamOnHGlobal.
	" Read data from connection "
	[ 
		read := connection readData.
		read isNil not 
	] whileTrue: [
		data nextPutAll: read asByteArray.
	].
	connection close.
	inet close.
	
	" Create bitmap "
	bmp := GdiplusBitmap new.
	bmp createImageOnIStream: data useEmbeddedColorManagement: false. " Access Violation "
	data Release.
	(bmp getType = 0 " ImageTypeUnknown (cannot add pool to this class)") 
		ifTrue: [ bmp delete. ^nil ].
	^bmp.! !

!AbtGraphicalObject * methods 09:04 - 10/14/04!
initialize
	" Do nothing " 
	visible := false.
	^self! !

!AbtGraphicalObject * methods 09:03 - 10/14/04!
localUpdate
	| draw |
	visible ifFalse: [ ^self ].

	draw := self parent drawable.
	self primitiveDrawOn: draw gc: draw.! !

!AbtGraphicalObject * methods 09:40 - 03/05/04!
parent
	" Return parent object for this control "
	" self class getWindowFromHandle: self getParent"
	^parent! !

!AbtGraphicalObject * methods 09:50 - 03/05/04!
parent: aWindow
	parent := aWindow.! !

!AbtGraphicalObject * methods 10:31 - 03/30/04!
update
	" Don't know how to implement this.
	  implementation probably looks like localUpdate "
	^self localUpdate.! !

!AbtGraphicalObject * methods 10:22 - 03/02/04!
visible
	^visible! !

!AbtGraphicalObject * methods 10:22 - 03/02/04!
visible: aBoolean
	visible := aBoolean! !

!AbtAppBldrView class * methods 08:47 - 03/11/04!
windowClassName
	^self name! !

!AbtAppBldrView * methods 10:34 - 03/16/04!
openWidget 
	self class registerClass.
	self preOpen.
	" super should be self here BUT a name clash between smalltalk's open and
	  cwic's open happened. In this method, we want to call smalltalk's open 
	  to open the window. That's why we call super open. "
	super open.
	self postOpen.! !

!AbtAppBldrView .CALLBACK methods 12:38 - 03/11/04!
WM_PAINT: hdc with: lparam
	"
	Private - Process the paint message. 
	Parameters:
		hdc      Handle to the device context to draw in. 
		lparam   Not used.
	Return Value:
		An application returns zero if it processes this message. 
	"
	| hDC ps oldPen |                     
	ps := PAINTSTRUCT localNew. 
	hDC := DeviceContext value: (WINAPI BeginPaint: m_handle with: ps basicAddress).      
"	oldPen := hDC selectSolidPen: (RGB red: 255 green: 255 blue: 255)."
	self drawOn: hDC.
"	hDC restoreObject: oldPen."
	WINAPI EndPaint: m_handle with: ps basicAddress.
	^NULL! !

Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: '.CALLBACK'A text: '.CALLBACK
This category marks methods that are called indirectly. 'A.!
