"use strict";
for ( var x of function * ( ) { yield * x ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */
