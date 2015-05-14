"Philemon SmalltalkMT exporter has done all the work
Version SstHttpsCommunications V 6.0.1  [49]"
PROJECTNAME SstHttpsCommunications .
PROJECTCATEGORY Philemon .
PREREQUISITES .
POOLS .

PROFILE
BEGIN
END

CLASS SstHttpsConnection
INCLUDEDEF=1
CLASSMETHODS
BEGIN
END
INSTANCEMETHODS
BEGIN
	negotiateConnect
	negotiateAccept
END

CLASS SstHttpsUrl
INCLUDEDEF=1
CLASSMETHODS
BEGIN
	cleanUpBeforeRemoveNonInherited
	setupAfterLoadNonInherited
END
INSTANCEMETHODS
BEGIN
	localEndpointUrl
END

! !

SstHttpConnection subclass: #SstHttpsConnection
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !
SstHttpUrl subclass: #SstHttpsUrl
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
	comment: '' 
	category: '' !	

!SstHttpsConnection publicMethods !

negotiateAccept
	"Negotiate SSL security first, if that fails go no further, if
	successful proceed to perform regular HTTP acceptance.
	Parameters : None
	Return value : self | <SstError>"

	| security acceptance rv |

	self sstToBeFixed.
	"Handle error from line below."
	security := self transport configuration securityConfiguration.

	((rv := self handle sslInitialize: security) isSslError 
	or: [ (rv := self handle sslConnect) isSslError 
	or: [ (rv := self handle sslVerifyCertificate) isSslError ]])
		ifTrue: [^SstError for: SstReceiveError with: rv].

	(acceptance := super negotiateAccept) isSstError
		ifTrue: [^acceptance].

	"Change the discriminator to include the security info so we don't share
	connections that differ only by their security."

	self peer discriminator: self handle socketIdHandle
!

negotiateConnect
	"Negotiate SSL security first, if that fails go no further, if
	successful proceed to perform regular HTTP connection.
	Parameters : None
	Return value : self | <SstError>"

	| err securityConfiguration |

	securityConfiguration := self transport configuration securityConfiguration.

	^((err := self handle sslInitialize: securityConfiguration) isSslError
		or: [(err := self handle sslAccept) isSslError])
			ifTrue: [SstError 
					for: SstReceiveError 
					with: ('Security negotiation failed: %1' bindWith: err printString)]
			ifFalse: [super negotiateConnect]
! !
	

!SstHttpsUrl class publicMethods !

cleanUpBeforeRemoveNonInherited

	SstUrl unregister: 'https'  "$NON-NLS$"!

setupAfterLoadNonInherited

	SstUrl register: 'https' as: self. "$NON-NLS$"

! !

!SstHttpsUrl publicMethods !

localEndpointUrl
	^'https:/httpsl/:0' sstAsUrl! !

