"use strict";
let x = { x = 0 } = 0n ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,1].PropertyDestructuringAssignmentEvaluation) but got normal */
