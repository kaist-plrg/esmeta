"use strict";
0 ^ [ { [ Symbol . toPrimitive ] : x => await } ] ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:ExpressionBody[0,0].Evaluation) but got transpile-failure */
