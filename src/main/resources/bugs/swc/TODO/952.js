"use strict";
let x = { x = x => 0 } = 0 ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,1].PropertyDestructuringAssignmentEvaluation) but got normal */
