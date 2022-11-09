"use strict";
class x extends null { static { throw x &&= 0 ; } } 

/* TAG: NEW-SWC-STATIC-REASSIGN-CLASS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(DeclarativeEnvironmentRecord.SetMutableBinding<SYNTAX>:AssignmentExpression[6,0].Evaluation)) but got throw-value: "unnamed:87: 0
        throw x && (x = 0);
        ^
" */
