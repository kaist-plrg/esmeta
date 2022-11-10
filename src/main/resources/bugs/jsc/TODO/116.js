"use strict";
do ; while ( [ ~ 0n , ] [ 0 ] &&= x ) ; 

/* TAG: NEW-JSC-ARR-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '&&=' must be a reference.) */
