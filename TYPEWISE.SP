" TypeWise load project "
PROJECTNAME TypeWise .
PROJECTCATEGORY  .
INITSCRIPT TypeWise_preload .


PROFILE
BEGIN
  BUILD.INCLUDESYMBOLTABLE = 0
  BUILD.APPNAME = ''
  BUILD.THREADINGMODEL = -1
  BUILD.OPTIMIZESTRUCTS = 0
  BUILD.RES = 1
  BUILD.CLEARGLOBALS = 0
  BUILD.DEBUGMODE = 0
  BUILD.EXCLUDESUPERMETHODS = 0
  BUILD.RWTEXT = 0
  BUILD.REGISTEROCX = 0
  BUILD.IMAGEFLAGS = 8
  BUILD.THREADINGMODEL.SYNCREQUIRED = 0
  BUILD.SPLITDEBUGSYMBOLS = 0
  BUILD.INITSERVERS = 0
  BUILD.BINDIMAGE = 0
  BUILD.BASEADDRESS = 400000
END

CLASS TypeWiseClass
INCLUDEDEF=1
CLASSMETHODS *
INSTANCEMETHODS *
! !
Object subclass: #TypeWiseClass
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	comment: ''
	category: ''!

!TypeWiseClass class * methods 12:49 - 10/13/04!
preBuildStep: anArg
	" self preBuildStep: nil "
 
	| fileName versionFile versionLine lastDot lastDoubleQuote fileContents version |
	fileName := 'D:\\smalltalk\\philemon-typewise-stmt\\res\\build\\clientversion.h'. 
	versionFile := FileStream read: fileName text: true.
	fileContents := (versionFile next: versionFile size) asStream.
	versionFile close.
	versionLine := fileContents nextLine asString.
	lastDot := versionLine lastIndexOf: $..
	lastDoubleQuote := versionLine lastIndexOf: $". 
	version := Integer newFromString: (versionLine copyFrom: lastDot + 1 to: lastDoubleQuote - 3).
	fileContents position: lastDot.
	fileContents nextPutAll: (version + 1) asString.
	fileContents nextPutAll: '\\0"'.
	versionFile := FileStream write: fileName text: true.
	versionFile nextPutAll: fileContents contents.
	versionFile close.! !

Compiler addCatDescription: '*'A text: '*
Default category.'A.!
