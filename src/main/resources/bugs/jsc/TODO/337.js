"use strict";
new function * x ( [ ] , x ) { } ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with destructuring parameters.) */
