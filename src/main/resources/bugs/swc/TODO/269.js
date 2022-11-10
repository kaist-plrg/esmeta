"use strict";
for ( var x of function * ( ) { yield * { [ Symbol . iterator ] : x => { } } ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetIterator ((step 4, 12:47-77))<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */
