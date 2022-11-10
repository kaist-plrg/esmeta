"use strict";
function await ( ) { } 0 > { [ Symbol . toPrimitive ] : x => await } ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:RelationalExpression[2,0].Evaluation) but got transpile-failure */
