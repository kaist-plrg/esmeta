"use strict";
new function x ( { 0 : x , } ) { } ( null <= { [ Symbol . toPrimitive ] : 0 || 0 } ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:RelationalExpression[3,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with destructuring parameters.) */
