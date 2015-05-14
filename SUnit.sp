"Philemon SmalltalkMT exporter has done all the work
Version SUnit Camp Smalltalk 3.1 RC10"
PROJECTNAME SUnit .
PROJECTCATEGORY Philemon_Exported_VASt_Application .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS TestCase
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	suite
	buildSuiteFromSelectors
	allTestSelectors
	selector:
	sunitVersion
	run:
	resources
	buildSuiteFromMethods:
	buildSuite
	debug:
	testSelectors
	suiteClass
	buildSuiteFromLocalSelectors
	isAbstract
	shouldInheritSelectors
	buildSuiteFromAllSelectors
END
INSTANCEMETHODS
BEGIN
	assert:description:resumable:
	isLogging
	deny:
	debug
	run:
	removeDependentFromHierachy:
	assert:description:
	should:
	tearDown
	signalFailure:
	should:raise:
	logFailure:
	should:raise:description:
	printOn:
	shouldnt:description:
	selector
	deny:description:
	deny:description:resumable:
	addDependentToHierachy:
	resources
	runCase
	assert:
	openDebuggerOnFailingTestMethod
	shouldnt:
	should:description:
	runCaseAsFailure:
	setUp
	failureLog
	shouldnt:raise:
	run
	shouldnt:raise:description:
	debugAsFailure
END

CLASS TestResource
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	isAvailable
	resources
	signalInitializationError
	isUnavailable
	current
	current:
	reset
	isAbstract
	new
END
INSTANCEMETHODS
BEGIN
	signalInitializationError
	description
	isUnavailable
	setUp
	resources
	description:
	isAvailable
	tearDown
	name:
	name
END

CLASS TestResult
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	error
	new
	exError
	signalErrorWith:
	resumableFailure
	signalFailureWith:
	failure
END
INSTANCEMETHODS
BEGIN
	isFailure:
	passed
	errors
	hasErrors
	failures
	printOn:
	isPassed:
	failureCount
	hasPassed
	passedCount
	runCount
	tests
	initialize
	isError:
	errorCount
	defects
	hasFailures
	runCase:
END

CLASS TestSuite
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	named:
END
INSTANCEMETHODS
BEGIN
	addTest:
	addTests:
	tests
	run:
	resources
	resources:
	defaultResources
	name:
	run
	addDependentToHierachy:
	name
	removeDependentFromHierachy:
END

! !

Object subclass: #TestCase
    instanceVariableNames: 'testSelector '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TestResource
    classInstanceVariableNames: 'current '
    instanceVariableNames: 'name description '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TestResult
    instanceVariableNames: 'failures errors passed '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
Object subclass: #TestSuite
    instanceVariableNames: 'tests resources name '
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!TestCase class publicMethods !

allTestSelectors

	^self sunitAllSelectors select: [:each | 'test*' sunitMatch: each]!

buildSuite
	| suite |
	^self isAbstract
		ifTrue: 
			[suite := self suiteClass named: self name asString.
			self allSubclasses 
				do: [:each | each isAbstract ifFalse: [suite addTest: each buildSuiteFromSelectors]].
			suite]
		ifFalse: [self buildSuiteFromSelectors]!

buildSuiteFromAllSelectors

	^self buildSuiteFromMethods: self allTestSelectors!

buildSuiteFromLocalSelectors

	^self buildSuiteFromMethods: self testSelectors!

buildSuiteFromMethods: testMethods

	^testMethods
		inject: (self suiteClass named: self name asString)
		into: [:suite :selector |
			suite
				addTest: (self selector: selector);
				yourself]!

debug: aSymbol

	^(self selector: aSymbol) debug!

isAbstract
	"Override to true if a TestCase subclass is Abstract and should not have
	TestCase instances built from it"

	^self sunitName = #TestCase!

resources

	^#()!

run: aSymbol

	^(self selector: aSymbol) run!

selector: aSymbol

	^self new setTestSelector: aSymbol!

shouldInheritSelectors
	"I should inherit from an Abstract superclass but not from a concrete one by default, unless I have no testSelectors in which case I must be expecting to inherit them from my superclass.  If a test case with selectors wants to inherit selectors from a concrete superclass, override this to true in that subclass."

	^self superclass isAbstract
		or: [self testSelectors isEmpty]

"$QA Ignore:Sends system method(superclass)$"!

suite

	^self buildSuite!

suiteClass
	^TestSuite!

sunitVersion
	^'3.1'!

testSelectors

	^self sunitSelectors select: [:each | 'test*' sunitMatch: each]! !

!TestCase publicMethods !

addDependentToHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"

!

assert: aBoolean

	aBoolean ifFalse: [self signalFailure: 'Assertion failed']!

assert: aBoolean description: aString
	aBoolean ifFalse: [
		self logFailure: aString.
		TestResult failure sunitSignalWith: aString]!

assert: aBoolean description: aString resumable: resumableBoolean 
	| exception |
	aBoolean
		ifFalse: 
			[self logFailure: aString.
			exception := resumableBoolean
						ifTrue: [TestResult resumableFailure]
						ifFalse: [TestResult failure].
			exception sunitSignalWith: aString]!

debug
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[(self class selector: testSelector) runCase] 
		sunitEnsure: [self resources do: [:each | each reset]]!

debugAsFailure
	| semaphore |
	semaphore := Semaphore new.
	self resources do: [:res | 
		res isAvailable ifFalse: [^res signalInitializationError]].
	[semaphore wait. self resources do: [:each | each reset]] fork.
	(self class selector: testSelector) runCaseAsFailure: semaphore.
!

deny: aBoolean

	self assert: aBoolean not!

deny: aBoolean description: aString
	self assert: aBoolean not description: aString!

deny: aBoolean description: aString resumable: resumableBoolean 
	self
		assert: aBoolean not
		description: aString
		resumable: resumableBoolean!

failureLog	
	^SUnitNameResolver defaultLogDevice
!

isLogging
	"By default, we're not logging failures. If you override this in 
	a subclass, make sure that you override #failureLog"
	^false!

logFailure: aString
	self isLogging ifTrue: [
		self failureLog 
			cr; 
			nextPutAll: aString; 
			flush]!

openDebuggerOnFailingTestMethod
	"SUnit has halted one step in front of the failing test method. Step over the 'self halt' and 
	 send into 'self perform: testSelector' to see the failure from the beginning"

	self
		halt;
		performTest!

printOn: aStream

	aStream
		nextPutAll: self class printString;
		nextPutAll: '>>#';
		nextPutAll: testSelector!

removeDependentFromHierachy: anObject 
	"an empty method. for Composite compability with TestSuite"

!

resources
	| allResources resourceQueue |
	allResources := Set new.
	resourceQueue := OrderedCollection new.
	resourceQueue addAll: self class resources.
	[resourceQueue isEmpty] whileFalse: [
		| next |
		next := resourceQueue removeFirst.
		allResources add: next.
		resourceQueue addAll: next resources].
	^allResources!

run
	| result |
	result := TestResult new.
	self run: result.
	^result!

run: aResult
	aResult runCase: self!

runCase

	[self setUp.
	self performTest] sunitEnsure: [self tearDown]!

runCaseAsFailure: aSemaphore
	[self setUp.
	self openDebuggerOnFailingTestMethod] sunitEnsure: [
		self tearDown.
		aSemaphore signal]!

selector
	^testSelector!

setUp!

should: aBlock
	self assert: aBlock value!

should: aBlock description: aString
	self assert: aBlock value description: aString!

should: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)!

should: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent)
		description: aString!

shouldnt: aBlock
	self deny: aBlock value!

shouldnt: aBlock description: aString
	self deny: aBlock value description: aString!

shouldnt: aBlock raise: anExceptionalEvent 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not!

shouldnt: aBlock raise: anExceptionalEvent description: aString 
	^self assert: (self executeShould: aBlock inScopeOf: anExceptionalEvent) not 		description: aString!

signalFailure: aString
	TestResult failure sunitSignalWith: aString

!

tearDown! !
	

!TestResource class publicMethods !

current

	current isNil
		ifTrue: [current := self new].

	^current!

current: aTestResource

	current := aTestResource!

isAbstract
	"Override to true if a TestResource subclass is Abstract and should not have
	TestCase instances built from it"

	^self name = #TestResource!

isAvailable
	^self current notNil and: [self current isAvailable]!

isUnavailable

	^self isAvailable not!

new

	^super new initialize!

reset

	current notNil ifTrue: [
		[current tearDown] ensure: [
			current := nil]]!

resources
	^#()!

signalInitializationError
	^TestResult signalErrorWith: 'Resource ' , self name , ' could not be initialized'! !

!TestResource publicMethods !

description

	description isNil
		ifTrue: [^''].

	^description!

description: aString

	description := aString!

isAvailable
	"override to provide information on the
	readiness of the resource"
	
	^true!

isUnavailable
	"override to provide information on the
	readiness of the resource"
	
	^self isAvailable not!

name

	name isNil
		ifTrue: [^self printString].

	^name!

name: aString

	name := aString!

resources
	^self class resources!

setUp
	"Does nothing. Subclasses should override this
	to initialize their resource"!

signalInitializationError
	^self class signalInitializationError!

tearDown
	"Does nothing. Subclasses should override this
	to tear down their resource"! !
	

!TestResult class publicMethods !

error
	^self exError!

exError
	^SUnitNameResolver errorObject!

failure
	^TestFailure!

new
	^super new initialize!

resumableFailure
	^ResumableTestFailure!

signalErrorWith: aString 
	self error sunitSignalWith: aString!

signalFailureWith: aString 
	self failure sunitSignalWith: aString! !

!TestResult publicMethods !

defects
	^OrderedCollection new
		addAll: self errors;
		addAll: self failures; yourself!

errorCount

	^self errors size!

errors

	errors isNil
		ifTrue: [errors := OrderedCollection new].
	^errors!

failureCount

	^self failures size!

failures
	failures isNil
		ifTrue: [failures := Set new].
	^failures!

hasErrors

	^self errors size > 0!

hasFailures

	^self failures size > 0!

hasPassed

	^self hasErrors not and: [self hasFailures not]!

initialize!

isError: aTestCase

	^self errors includes: aTestCase!

isFailure: aTestCase
	^self failures includes: aTestCase!

isPassed: aTestCase

	^self passed includes: aTestCase!

passed

	passed isNil
		ifTrue: [passed := OrderedCollection new].

	^passed !

passedCount

	^self passed size!

printOn: aStream

	aStream
		nextPutAll: self runCount printString;
		nextPutAll: ' run, ';
		nextPutAll: self correctCount printString;
		nextPutAll: ' passed, ';
		nextPutAll: self failureCount printString;
		nextPutAll: ' failed, ';
		nextPutAll: self errorCount printString;
		nextPutAll: ' error'.

	self errorCount ~= 1
		ifTrue: [aStream nextPut: $s]!

runCase: aTestCase

	| testCasePassed |

	testCasePassed :=
		[
			[
				aTestCase runCase.
				true]
					sunitOn: self class failure
					do: [:signal |
						self failures add: aTestCase.
						signal sunitExitWith: false]]
							sunitOn: self class error
							do: [:signal |
								self errors add: aTestCase.
								signal sunitExitWith: false].

	testCasePassed
		ifTrue: [self passed add: aTestCase]!

runCount

	^self passedCount + self failureCount + self errorCount!

tests

	^(OrderedCollection new: self runCount)
		addAll: self passed;
		addAll: self errors;
		addAll: self failures;
		yourself! !
	

!TestSuite class publicMethods !

named: aString

	^self new
		name: aString;
		yourself! !

!TestSuite publicMethods !

addDependentToHierachy: anObject
	self sunitAddDependent: anObject.
	self tests do: [ :each | each addDependentToHierachy: anObject]!

addTest: aTest
	self tests add: aTest!

addTests: aCollection 
	aCollection do: [:eachTest | self addTest: eachTest]!

defaultResources
	^self tests 
		inject: Set new
		into: [:coll :testCase | 
			coll
				addAll: testCase resources;
				yourself]!

name

	^name!

name: aString

	name := aString!

removeDependentFromHierachy: anObject
	self sunitRemoveDependent: anObject.
	self tests do: [ :each | each removeDependentFromHierachy: anObject]!

resources
	resources isNil ifTrue: [resources := self defaultResources].
	^resources!

resources: anObject
	resources := anObject!

run
	| result |
 	result := TestResult new.
	self resources do: [ :res |
		res isAvailable ifFalse: [^res signalInitializationError]].
	[self run: result] sunitEnsure: [self resources do: [:each | each reset]].
	^result!

run: aResult 
	self tests do: [:each | 
		self sunitChanged: each.
		each run: aResult]!

tests
	tests isNil ifTrue: [tests := OrderedCollection new].
	^tests! !

