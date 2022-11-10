"use strict";
{ x = [ ~ 0 , ] [ 0 ] /= 0 ; } 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(PutValue ((step 4.a, 6:45-80))<SYNTAX>:AssignmentExpression[4,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '/=' must be a reference.) */
