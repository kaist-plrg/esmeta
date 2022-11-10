"use strict";
x in x ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:RelationalExpression[6,0].Evaluation) but got throw-error: TypeError(unnamed:4: TypeError: Cannot use 'in' operator to search for 'undefined' in undefined) */
