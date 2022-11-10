"use strict";
0 . x ??= x ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[8,0].Evaluation) but got throw-error: TypeError(unnamed:6: TypeError: Cannot create property 'x' on number '0') */
