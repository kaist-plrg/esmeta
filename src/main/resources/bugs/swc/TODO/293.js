"use strict";
let x = x . x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:MemberExpression[2,0].Evaluation) but got throw-error: TypeError(unnamed:4: TypeError: Cannot read properties of undefined (reading 'x')) */
