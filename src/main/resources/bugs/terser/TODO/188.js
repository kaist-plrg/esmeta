"use strict";
x ?. x ; let x ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:OptionalExpression[0,0].Evaluation) but got normal */
