"use strict";
const x = 0 ; [ x ] = `` ; 

/* TAG: NEW-SWC-ASSIGN-CONST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentElement[0,0].IteratorDestructuringAssignmentEvaluation) but got transpile-failure */
