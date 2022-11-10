"use strict";
0 . x |= x ; let x ; 

/* TAG: NEW-YET-TRS-REMOVE-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[5,0].Evaluation) but got throw-error: TypeError(unnamed:3: TypeError: Cannot create property 'x' on number '0') */
