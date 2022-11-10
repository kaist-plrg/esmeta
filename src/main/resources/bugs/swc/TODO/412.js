"use strict";
const x = 0 ; [ { x } = 0 ^ 0 ] = `` ; 

/* TAG: NEW-SWC-ASSIGN-CONST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,0].PropertyDestructuringAssignmentEvaluation) but got transpile-failure */
