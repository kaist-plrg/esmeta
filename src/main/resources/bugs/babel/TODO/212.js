"use strict";
for ( let x of { [ Symbol . iterator ] : function * ( ) { yield * [ 0 , ] [ 0 ] ; } } ) ; 

/* TAG: NEW-DSG-HIDDEN-GEN-PARAM-ERROR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Call ((step 2, 3:43-73))<SYNTAX>:YieldExpression[2,0].Evaluation) but got normal */
