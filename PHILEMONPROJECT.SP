""
PROJECTNAME PhilemonProject .
PROJECTCATEGORY "Philemon Smalltalk MT code" .

CLASS Object
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	initialize
	initializeAfterLoad
END
INSTANCEMETHODS 
BEGIN
END

CLASS ProjectsLoader
INCLUDEDEF=1
CLASSMETHODS 
BEGIN
	postloadPostfix
	preloadPostfix
	testProjectPostfix
	fileExtension
END
INSTANCEMETHODS 
BEGIN
	loadProjectsFromFilesNamed:
	initializeProject:
	libPath
	loadVAProjectFromFile:
	libPath:
	output:
	loadStMtProjectFromFile:additionalPrerequisites:
	loadStMtProjectFromFile:
	includeTests:
	includeTests
END

CLASS IDE::DevelopmentEnvironment
INCLUDEDEF=0
CLASSMETHODS 
BEGIN
	processCompilerOptions:
END
INSTANCEMETHODS 
BEGIN
END
! !
Object subclass: #ProjectsLoader
	instanceVariableNames: 'libPath debugOut includeTests '
	classVariableNames: ''
	poolDictionaries: 'WinUserConstants _StImageConstants '
	comment: ''
	category: ''!

!Object class publicMethods methods 02:19 - 02/23/04!
initialize
	^self! !

!Object class publicMethods methods 02:19 - 02/23/04!
initializeAfterLoad
	^self! !

!ProjectsLoader class * methods 12:10 - 02/24/04!
fileExtension
	^'.sp'! !

!ProjectsLoader class * methods 12:10 - 02/24/04!
postloadPostfix
	^'StMt_Postload'! !

!ProjectsLoader class * methods 12:10 - 02/24/04!
preloadPostfix
	^'StMt_Preload'! !

!ProjectsLoader class * methods 12:10 - 02/24/04!
testProjectPostfix
	^'Test'! !

!ProjectsLoader * methods 06:49 - 09/05/05!
includeTests
	^includeTests == true! !

!ProjectsLoader * methods 06:49 - 09/05/05!
includeTests: bool
	includeTests := bool! !

!ProjectsLoader * methods 12:09 - 02/24/04!
initializeProject: aProjectDescriptor
	aProjectDescriptor classes do: [ :each | | cls |
		cls := Smalltalk at: each className asSymbol . 
		cls isNil ifFalse: [
			each includesDef ifTrue: [
				cls initialize.
				cls initializeAfterLoad.
				].
			].
		].! !

!ProjectsLoader * methods 02:48 - 02/23/04!
libPath
	^libPath! !

!ProjectsLoader * methods 02:44 - 02/23/04!
libPath: aPathString
	libPath := aPathString! !

!ProjectsLoader * methods 12:20 - 02/24/04!
loadProjectsFromFilesNamed: projectFileNamesOrderedCollection
	projectFileNamesOrderedCollection do: [:each |
		self loadVAProjectFromFile: each ].! !

!ProjectsLoader * methods 01:03 - 08/19/04!
loadStMtProjectFromFile: aProjectFileNameWithCase
	| projectDescriptor projectLoader aProjectFileName |
	aProjectFileName := (File locateFile: aProjectFileNameWithCase libPaths: self libPath).
	aProjectFileName isNil ifTrue: [
		self output: aProjectFileNameWithCase, ' could not be found on the path.'.
		^nil
	].
	aProjectFileName := aProjectFileName asUppercase.
	(ProjectManager loadedProjects keyAtValue: aProjectFileName) isNil ifFalse: [
		self output: aProjectFileNameWithCase, ' already loaded.'. 
		^nil.
		].
	self output: 'Trying to load ', aProjectFileName.
	projectDescriptor := ProjectManager openProject: aProjectFileName fLoadSource: false parentWindow: nil.
	projectDescriptor isNil ifTrue: [ 
		self output: '-- failed!! '.
		self halt.
		^nil ].
	projectLoader := Core::ProjectLoader new.
	projectLoader setProject: projectDescriptor 
		path: aProjectFileName
		searchDirectory: self libPath 
		searchProjects: nil
		parentWindow: nil
		notifierWindow: nil.
 	projectLoader installProject: ST_FILEIN_COMPILE|ST_FILEIN_EVALUATE.
 	ProjectManager addLoadedProject: projectDescriptor fileName: aProjectFileName.
 	projectDescriptor loadProject: ST_FILEIN_LOAD.
	self output: 'success!! '.
	^projectDescriptor! !

!ProjectsLoader * methods 12:51 - 08/19/04!
loadStMtProjectFromFile: aProjectFileNameWithCase additionalPrerequisites: projectDescriptorCollection
	| projectDescriptor projectLoader aProjectFileName prqSet |
	aProjectFileName := (File locateFile: aProjectFileNameWithCase libPaths: self libPath).
	aProjectFileName isNil ifTrue: [
		self output: aProjectFileNameWithCase, ' could not be found on the path.'.
		^nil
	].
	aProjectFileName := aProjectFileName asUppercase.
	(ProjectManager loadedProjects keyAtValue: aProjectFileName) isNil ifFalse: [
		self output: aProjectFileNameWithCase, ' already loaded.'. 
		^nil.
		].
	self output: 'Trying to load ', aProjectFileName.
	projectDescriptor := ProjectManager openProject: aProjectFileName fLoadSource: false parentWindow: nil.
	projectDescriptor isNil ifTrue: [ 
		self output: '-- failed!! '.
		self halt.
		^nil ].
	projectLoader := Core::ProjectLoader new.
	projectLoader setProject: projectDescriptor 
		path: aProjectFileName
		searchDirectory: self libPath 
		searchProjects: nil
		parentWindow: nil
		notifierWindow: nil.

" 	projectDescriptorCollection do: [ :each | each isNil ifFalse: [ projectLoader addPrerequisites: each name: each projectName ] ]."
    prqSet := Dictionary new.
 	projectDescriptorCollection do: [ :each | each isNil ifFalse: [ prqSet at: each projectName put: each ] ].

 	projectLoader installProject: ST_FILEIN_COMPILE|ST_FILEIN_EVALUATE prerequisites: prqSet loadPath: self libPath asStream. 
 	"ProjectManager addLoadedProject: projectDescriptor fileName: aProjectFileName."
 	projectDescriptor loadProject: ST_FILEIN_LOAD. 
	self output: 'success!! '.
	^projectDescriptor! !

!ProjectsLoader * methods 06:51 - 09/05/05!
loadVAProjectFromFile: projectFileName
	"
	Do not include extension.
	
	Steps:
		1- load preload project.
		2- load project
		3- load postload project
		4- run initialize and initializeAfterLoad 
	"
	|loadedProject preLoadedProject postLoadedProject |
	preLoadedProject := self loadStMtProjectFromFile: projectFileName, self class preloadPostfix, self class fileExtension additionalPrerequisites: OrderedCollection new.
	loadedProject := self loadStMtProjectFromFile: projectFileName, self class fileExtension additionalPrerequisites: (OrderedCollection with: preLoadedProject).
	postLoadedProject := self loadStMtProjectFromFile: projectFileName, self class postloadPostfix, self class fileExtension additionalPrerequisites: OrderedCollection new. 
	loadedProject isNil ifFalse: [
		self initializeProject: loadedProject.
		preLoadedProject ifNotNil: [ :prj | loadedProject addPrerequisiteProject: prj projectName ]. 
		postLoadedProject ifNotNil: [ :prj | loadedProject addPrerequisiteProject: prj projectName ]. 
	].
	self includeTests ifTrue: [ 
		self loadStMtProjectFromFile: projectFileName, self class testProjectPostfix, self class fileExtension.
		self loadStMtProjectFromFile: projectFileName, self class testProjectPostfix, self class postloadPostfix, self class fileExtension.
	].! !

!ProjectsLoader * methods 06:57 - 10/24/05!
output: aString
	debugOut isNil ifTrue: [
		Transcript isNil ifFalse: [ debugOut := Transcript ].
	].
	debugOut isNil 
		ifTrue: [ Processor outputDebugLine: aString ]
		ifFalse: [ debugOut nextPutAll: aString ;cr].! !

!IDE::DevelopmentEnvironment class $Compiler Interface methods 10:45 - 06/04/04!
processCompilerOptions: cmdLineArgs 
	"
	Private - Processes command-line options for the compiler.  
	Parameters: 
		cmdLineArgs    Command-line arguments array.
	Return Value: 
		This method does not return a value.	
	Remarks: 
		The following options are supported:
		-compile <project>
		-imports <list of library directories>
		-output  <output directory>
		-M1 -M2 -M3 Message level in the generated executable	
	"  
	|options libPath currentDir prj projects projectDir output updateCollection flagSpecs loadScript|
	Compiler closeLog.	 
	options := Processor parseCommandOptions: cmdLineArgs.  
	currentDir := File getCurrentDirectory.     
	projects := options at: '-compile' ifNone: EmptyArray.
	libPath  := (options at: '-imports' ifNone: EmptyArray),
		thisApplication loadLibraryPath. 
	output := options at: '-output' ifNone: currentDir.
	output ~~ currentDir ifTrue: [output := output first].
	loadScript := options at: '-script' ifNone: nil. 
	prj := projects at: 1.   
	(File isRoot: prj) ifFalse: [
		prj := File makePathname: File getCurrentDirectory filename: prj.
	].                                 

	loadScript isNil 
	ifFalse: [ | content |
	 	Processor outputDebugLine: 'Loading script ', loadScript.
		content := (File loadStringContents: loadScript).
		content isNil ifFalse: [ Compiler evaluate: content ] ].
         
	flagSpecs := self processMessageFlags: options.         
	flagSpecs notNil ifTrue: [
		updateCollection := OrderedCollection new.	
		flagSpecs basicDo: [ :i :rg |
		Compiler updateSmalltalkConstant: (rg at: 1)
			value: (rg at: 2)
			update: updateCollection. 
		].
		Compiler recompileMethods: updateCollection.
	].

	projectDir := File getPathName: prj. 
	ProjectBuilder new buildImage: prj
		libPath: libPath             
		currentDirectory: projectDir
		outputDirectory: output! !

Compiler addCatDescription: 'publicMethods'A text: 'publicMethods'A.
Compiler addCatDescription: '*'A text: '*
Default category.'A.
Compiler addCatDescription: ' Compiler Interface'A text: ' Compiler Interface'A.!
