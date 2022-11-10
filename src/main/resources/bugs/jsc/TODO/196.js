"use strict";
let [ ] = ~ async function * x ( ... { x } ) { } ( ) [ 0 >> null . x ] ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:ShiftExpression[2,0].Evaluation) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with destructuring parameters.) */
