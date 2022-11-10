"use strict";
do ; while ( x ) ; let x ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(DeclarativeEnvironmentRecord.GetBindingValue<SYNTAX>:DoWhileStatement[0,0].DoWhileLoopEvaluation) but got normal */
