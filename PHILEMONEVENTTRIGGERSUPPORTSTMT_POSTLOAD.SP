""
PROJECTNAME "PhilemonEventTriggerSupport Postload" .
PROJECTCATEGORY "Philemon Event Trigger Support" .


PROFILE
BEGIN
END

CLASS Object
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
END
INSTANCEMETHODS 
BEGIN
	when:send:to:
END
! !

!Object publicMethods methods 06:38 - 01/29/04!
when: anEventNameSymbol send: aSelectorSymbol to: anObject
	"Form an action with <anObject> as the receiver and
	a <aSelectorSymbol> as the message selector and append
	it to the actions list for the event named <anEventNameSymbol>."
	
	self when: anEventNameSymbol send: aSelectorSymbol to: anObject withArguments: #()! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.!
