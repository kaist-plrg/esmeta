"use strict";
switch ( 1 ) { case { x } = 0 : default : } let x ; 

/* TAG: NEW-SWC-CHAIN-ASSIGN-OBJ
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentProperty[0,0].PropertyDestructuringAssignmentEvaluation) but got normal */
