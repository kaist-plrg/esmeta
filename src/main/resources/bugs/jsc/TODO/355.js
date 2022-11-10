"use strict";
throw `${ 0 ? 0 : 0 }` [ 0 ] . x [ 0 ] /= 1 ; 

/* TAG: NEW-JSC-TMP-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:AssignmentExpression[5,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '/=' must be a reference.) */
