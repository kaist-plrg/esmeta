"use strict";
const x = 0 ; for ( { x , } in [ 0 ] ) ; 

/* TAG: NEW-GRL-LOGICAL-ASSIGN-CONST
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,0].PropertyDestructuringAssignmentEvaluation) but got normal */
