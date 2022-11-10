"use strict";
for ( let x in x . x ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:MemberExpression[2,0].Evaluation) but got throw-error: TypeError(unnamed:5: TypeError: Cannot read properties of undefined (reading 'x')) */
