"use strict";
for ( let x in [ x ] ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ElementList[0,0].ArrayAccumulation) but got normal */
