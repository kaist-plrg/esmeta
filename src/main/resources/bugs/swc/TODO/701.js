"use strict";
for ( var x of function * ( ) { yield * await ; } ( ) ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:YieldExpression[2,0].Evaluation) but got transpile-failure */
