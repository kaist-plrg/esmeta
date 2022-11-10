"use strict";
{ x = this . x ++ ; } 

/* TAG: NEW-ECM-STRICT-X-THIS-X
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(PutValue ((step 4.a, 6:45-80))<SYNTAX>:AssignmentExpression[4,0].Evaluation) but got normal */
