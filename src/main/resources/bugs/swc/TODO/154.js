"use strict";
for ( var x of function * x ( ) { yield * { [ Symbol . iterator ] : x => [ ] } ; } ( ) ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */
