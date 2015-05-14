""
PROJECTNAME "PhilemonTerminalViewTestStMt Postload" .
PROJECTCATEGORY "Philemon Terminal View" .

CLASS TerminalTests
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	testImageBackground
	testImageBackgroundFromFile
	testTimer
END
! !

!TerminalTests publicMethods methods 11:39 - 03/30/04!
testImageBackground

	| t  s x image|
	t := Terminal new.
	t openWidget.
	s := TerminalWidget windowIn: (1 @ 1 extent: 40 @ 80).
	x := TerminalWidget textIn: (1 @ 1 extent: 10 @ 10).
	image := TerminalWidget image: 'http://www.goodlogo.com/logos/small/ns.gif' in: (1@1 extent: 4@4).

	s add: x.
	s add: image.
	t show: s.
	t gettingFocus.
	
	self close: t! !

!TerminalTests publicMethods methods 11:39 - 03/30/04!
testImageBackgroundFromFile

	| t  s x image|
	t := Terminal new.
	t openWidget.
	s := TerminalWidget windowIn: (1 @ 1 extent: 40 @ 80).
	x := TerminalWidget textIn: (1 @ 1 extent: 10 @ 10).
	image := TerminalWidget image: 'c:\\ns.gif' in: (1@1 extent: 4@4).

	s add: x.
	s add: image.
	t show: s.
	t gettingFocus.
	
	self close: t! !

!TerminalTests publicMethods methods 10:53 - 10/05/04!
testTimer
	|t s counter |
	s := TerminalWidget windowClass in: ( 1 @ 1 extent: 20 @ 20 ).
	counter := TerminalTimer in: (2@2 extent: 0@5).
	counter interval: (2 to: 20 by: 2).
	counter delayInSeconds: 1.
	counter repeat: true.
	s add: counter.            
            
"	c1 := TerminalLabel in: (2@2 extent: 0@10).
	c1 appearance background: Color blue.
	c1 string: 'Left'.
	c1 alignment: 1.
	s add: c1.

	c2 := TerminalLabel in: (4@2 extent: 0@10).
	c2 appearance background: Color blue.
	c2 string: 'Middle'.
	c2 alignment: 2.
	s add: c2.

	c3 := TerminalLabel in: (6@2 extent: 0@10).
	c3 appearance background: Color blue.
	c3 string: 'Right'.
	c3 alignment: 3.
	s add: c3."
	
	t := Terminal show: s.
	self close: t! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
