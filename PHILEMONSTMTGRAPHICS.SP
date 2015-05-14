""
PROJECTNAME PhilemonStMtGraphics .
PROJECTCATEGORY  .

CLASS CgDrawable
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *

CLASS FrameWindow
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	title:
END

CLASS Font
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	metrics
END
! !
Object subclass: #CgDrawable
	instanceVariableNames: 'target pens originalPen foreground 
'
	classVariableNames: ''
	poolDictionaries: 'WinGDIConstants WinUserConstants ColorConstants WinGdiPlusConstants WinGDIConstants '
	comment: ''
	category: ''!

!CgDrawable class * methods 02:20 - 03/08/04!
for: aDeviceContext
	^self new target: aDeviceContext; initialize; yourself.! !

!CgDrawable * methods 12:54 - 10/15/04!
drawArc: aCgDrawable x: x y: y width: width height: height angle1: angle1 angle2: angle2 

	| box |
	box := WinRectangle localNew.
	box left: x top: y width: width height: height.
	self target ellipse: box.
	" self target arc: ((x @ y) extent: (width @ height)) startArc: (x @ y) endArc: (x @ y) "! !

!CgDrawable * methods 12:53 - 10/15/04!
drawLine: aCgDrawable x1: xStart y1: yStart x2: xStop y2: yStop   
	| begin end |
	begin := WinPoint localNew.
	begin x: xStart y: yStart.
	end := WinPoint localNew.
	end x: xStop y: yStop .
	" The to-pixel is not set. So we will need to set it separately "
	target lineFrom: begin to: end.
    target setPixel_x: xStop y: yStop color: foreground.! !

!CgDrawable * methods 02:44 - 10/15/04!
drawRectangle: aCgDrawable x: x y: y width: width height: height 
	| leftTop leftBottom rightTop rightBottom |
	leftTop := WinPoint localNew.
	leftTop x: x y: y.
	leftBottom := WinPoint localNew.
	leftBottom x: x y: (y + height).
	rightTop := WinPoint localNew.
	rightTop x: (x + width - 1) y: y.
	rightBottom := WinPoint localNew.
	rightBottom x: (x + width - 1) y: (y + height).
	target lineFrom: leftTop to: rightTop.   
	target lineFrom: rightTop to: rightBottom.
	target lineFrom: rightBottom to: leftBottom.
	target lineFrom: leftBottom to: leftTop.! !

!CgDrawable * methods 02:34 - 10/15/04!
drawString: aCgDrawable boundedBy: aWinRectangle string: aStringA 
  	| oldBk |
  	oldBk := self target setBkMode: TRANSPARENT.
 	self target textOut: aStringA x: aWinRectangle left y: aWinRectangle top + 1.
 	(oldBk = 0) ifFalse: [ self target setBkMode: oldBk ].! !

!CgDrawable * methods 09:52 - 04/22/04!
drawString: aCgDrawable x: anInteger y: anInteger2 string: aStringA 
	target textOut: aStringA x: anInteger y: anInteger2! !

!CgDrawable * methods 01:29 - 10/15/04!
fillRectangle: aCgDrawable x: x y: y width: width height: height
	|box|
	box := WinRectangle localNew.
	box left: x top: y width: width height: height. 
	target fillRect: box color: self getForeground.! !

!CgDrawable * methods 02:24 - 03/08/04!
getBackground
    "anInteger := RGB red: 255 green: 0 blue: 0."
	^target getBkColor! !

!CgDrawable * methods 12:37 - 06/16/04!
getForeground
	foreground isNil ifTrue: [ ^target getBkColor ].
	^foreground! !

!CgDrawable * methods 08:52 - 10/06/04!
initialize
	pens := SystemDictionary new.   
	"originalPen := target selectSolidPen: (RGB red: 0 green: 0 blue: 0)."! !

!CgDrawable * methods 11:15 - 03/10/04!
penColoured: anIntColor
	^pens at: anIntColor ifAbsentPut: [ Pen createPen: PS_SOLID width: 1 color: anIntColor ].! !

!CgDrawable * methods 09:07 - 04/07/04!
putDeviceIndependentImage: gc 
	image: image 
	srcRect: srcRect
	destRect: destRect
	
	" StMt can draw an image in jpeg or bmp format using GdiPlus "
	" We receive an image (in 'image') but we need to fit this in destRect."
	" Since we scale the image down to the destination rectangle, the value in
	  srcRect is ignored. "

	| gdiplusGraphics |
	gdiplusGraphics := GdiplusGraphics new hDC: self target.
	gdiplusGraphics drawImage: image x: destRect origin x y: destRect origin y cx: destRect width cy: destRect height.
	gdiplusGraphics delete.! !

!CgDrawable * methods 11:35 - 03/10/04!
release
	originalPen isNil ifFalse: [ target selectObject: originalPen ].
	pens do: [ : pen | pen delete ].
	pens := SystemDictionary new.
	target ifNotNil: [ :contxt | contxt delete ].
	target := nil.! !

!CgDrawable * methods 01:47 - 03/08/04!
setBackground: anInteger 
    "anInteger := RGB red: 255 green: 0 blue: 0."
	^target setBkColor: anInteger! !

!CgDrawable * methods 11:20 - 10/18/04!
setFont: aFont                     
	target selectObject: aFont.! !

!CgDrawable * methods 11:01 - 03/10/04!
setForeground: anInteger                    
	| newPen |
	(anInteger = foreground) ifTrue: [ ^self ].
	newPen := self penColoured: anInteger.
	" anInteger := RGB red: 0 green: 0 blue: 256. "
	" pen := Pen createPen: PS_SOLID width: 1 color: anInteger."
	"(Pen new value: (target selectObject: pen)) delete." 
	originalPen isNil ifTrue: [
		originalPen := target selectObject: newPen. 
	] ifFalse: [
		target selectObject: newPen.
	].
	target setTextColor: anInteger.
	foreground := anInteger! !

!CgDrawable Accessing methods 10:43 - 03/08/04!
target
	^target! !

!CgDrawable Accessing methods 09:05 - 03/08/04!
target: aDeviceContext
	target := aDeviceContext! !

!FrameWindow File Handling methods 09:59 - 06/03/04!
title: szTitle
	"
	Sets the title of the receiver.
	Parameters:
		szTitle    A String with the title text to display, or nil for untitled.
	Return Value:
		This method does not return a value.
	Remarks:
		The displayed title text is constructed from the prefix and the given title.
	"
	| szAppName |
	szAppName := self windowTitle.
	szTitle isNil ifTrue: [
		(szAppName = NULL) 
		ifFalse: [ WINAPI SetWindowText: m_handle with: 'Untitled - ',szAppName ] 
		ifTrue: [ WINAPI SetWindowText: m_handle with: 'Untitled' ].
		m_properties removeKey: #document.
		m_menu ~~ NULL ifTrue: [
			m_menu 
				enableItem: ID_FILE_SAVE state: FALSE;
				enableItem: ID_FILE_PRINT state: FALSE;
				enableItem: ID_FILE_PRINT_PREVIEW state: FALSE.
		].
		^true
	].
	(szAppName = NULL)
		ifFalse: [ 	WINAPI SetWindowText: m_handle with: (szTitle,' - ',szAppName) ]
		ifTrue: [ WINAPI SetWindowText: m_handle with: szTitle ].
	m_properties at: #document put: szTitle.
	m_menu ~~ NULL ifTrue: [m_menu enableItem: ID_FILE_SAVE state: TRUE]! !

!Font Operations methods 02:19 - 04/23/04!
metrics
	| metr dc oldFont |  
	metr := TEXTMETRIC new.
	dc := DeviceContext fromHandle: (WINAPI GetDC: HWND_DESKTOP).
	oldFont := dc selectObject: self.    
	WINAPI GetTextMetrics: dc with: metr basicAddress.
	dc selectObject: oldFont.
	WINAPI ReleaseDC: HWND_DESKTOP with: dc.
	^metr! !

Compiler addCatDescription: 'Accessing'A text: 'Accessing
Accesses properties of the object.'A.
Compiler addCatDescription: 'Operations'A text: 'Operations
Operations on this object'A.
Compiler addCatDescription: 'File Handling'A text: 'File Handling
Document handling methods.'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.!
