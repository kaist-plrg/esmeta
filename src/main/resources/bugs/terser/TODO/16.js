"use strict";
x ?? 0 ; let x ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:CoalesceExpression[0,0].Evaluation) but got normal */
