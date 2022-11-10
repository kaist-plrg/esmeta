"use strict";
[ ... [ { ... `${ 0 ^ 0 == 0 >>> 0 == 0 ^ new [ 0 ] [ 0 , 0 ] == 0 }` [ 0 ] /= 0 , } ^ 0 ] , ] ; 

/* TAG: NEW-JSC-TMP-LITERAL-LHS
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Left hand side of operator '/=' must be a reference.) */
