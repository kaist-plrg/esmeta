"use strict";
let x = x => function ( ) { } ?. ( ) . x ; x ( ) ; 

/* TAG: NEW-JSC-OPTIONAL-CALL-CHAIN
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:ExpressionBody[0,0].Evaluation) but got normal */
