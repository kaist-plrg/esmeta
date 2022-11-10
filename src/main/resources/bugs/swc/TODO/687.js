"use strict";
for ( let x of x ??= 0 ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[8,0].Evaluation) but got throw-error: TypeError(unnamed:20: TypeError: (intermediate value)(intermediate value)(intermediate value)[Symbol.iterator] is not a function) */
