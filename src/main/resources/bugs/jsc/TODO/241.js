"use strict";
for ( var x in function * ( ) { } ( ) [ async function * x ( ... x ) { } . x ] ) ; 

/* TAG: NEW-JSC-FUNC-PARAM-SAME-NAME
[Exit Tag Mismatch]
 > Expected normal but got throw-error: SyntaxError(Exception: SyntaxError: Duplicate parameter 'x' not allowed in function with a rest parameter.) */
