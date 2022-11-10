"use strict";
new function x ( [ ] = new new . target ( `` ) ( ) , x ) { } ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateCall ((step 5, 13:48-78))<SYNTAX>:CallExpression[0,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with default parameter values.) */
