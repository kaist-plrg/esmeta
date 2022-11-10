"use strict";
x instanceof x ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:RelationalExpression[5,0].Evaluation) but got throw-error: TypeError(unnamed:8: TypeError: Right-hand side of 'instanceof' is not an object) */
