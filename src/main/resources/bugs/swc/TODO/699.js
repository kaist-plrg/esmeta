"use strict";
const x = 0 ; [ { 0 : x } = 0 ] = '' ; 

/* TAG: NEW-SWC-ASSIGN-CONST-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentElement[0,0].KeyedDestructuringAssignmentEvaluation) but got transpile-failure */
