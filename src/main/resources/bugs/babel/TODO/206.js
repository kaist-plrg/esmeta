"use strict";
for ( let x in new x ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:NewExpression[1,0].Evaluation) but got throw-error: TypeError(unnamed:5: TypeError: x is not a constructor) */
