"use strict";
for ( var x of function * ( ) { yield * { [ Symbol . iterator ] : 0 } ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(GetMethod ((step 3, 4:46-76))<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */
