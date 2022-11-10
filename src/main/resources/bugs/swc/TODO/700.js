"use strict";
{ x ( ) ; let x ; } 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: TypeError(unnamed:5: TypeError: x is not a function) */
