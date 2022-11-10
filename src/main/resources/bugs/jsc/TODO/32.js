"use strict";
0 & 0 [ async function * x ( [ x , ] ) { } ( ) `` ] ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:BindingPattern[1,0].BindingInitialization) but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with destructuring parameters.) */
