"use strict";
for ( var x of function * ( ) { yield x ; let x ; } ( ) ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:YieldExpression[1,0].Evaluation) but got normal */
