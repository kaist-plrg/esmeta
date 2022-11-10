"use strict";
x = { x } = 0 ; let x ; 

/* TAG: NEW-SWC-CHAIN-ASSIGN-OBJ
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,0].PropertyDestructuringAssignmentEvaluation) but got normal */
