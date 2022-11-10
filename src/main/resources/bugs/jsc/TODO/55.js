"use strict";
for ( ; async function x ( [ x , ] ) { } ( ) ( ) ; 0 ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateCall ((step 5, 13:48-78))<SYNTAX>:CallExpression[3,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with destructuring parameters.) */
