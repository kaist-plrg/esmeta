"use strict";
{ new x ( ) ; let x ; } 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:MemberExpression[6,0].Evaluation) but got throw-error: TypeError(unnamed:3: TypeError: x is not a constructor) */
