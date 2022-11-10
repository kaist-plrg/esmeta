"use strict";
const x = 0 ; [ x = x => 0 ] = `` ; 

/* TAG: NEW-SWC-ASSIGN-CONST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentElement[0,1].IteratorDestructuringAssignmentEvaluation) but got transpile-failure */
