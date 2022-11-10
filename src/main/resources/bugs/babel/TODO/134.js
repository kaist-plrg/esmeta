"use strict";
for ( let x of x ??= 0 ) ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:AssignmentExpression[8,0].Evaluation) but got throw-error: TypeError(unnamed:7: TypeError: Invalid attempt to iterate non-iterable instance.) */
