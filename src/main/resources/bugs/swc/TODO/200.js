"use strict";
let x = { [ 0 || x ] : x } <= 0 ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:LogicalORExpression[1,0].Evaluation) but got normal */
