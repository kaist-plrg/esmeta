"use strict";
0 ( 0 , ... `${ 0 == 0 }` ( ) . x %= 0 ) ; 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateCall ((step 4, 12:45-75))<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '%=' must be a reference.) */
