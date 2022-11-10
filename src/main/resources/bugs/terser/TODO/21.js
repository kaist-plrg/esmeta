"use strict";
for ( var x of function * ( ) { return x ; let x ; } ( ) ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:ReturnStatement[1,0].Evaluation) but got normal */
