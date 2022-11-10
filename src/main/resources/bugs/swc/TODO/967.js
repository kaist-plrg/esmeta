"use strict";
`` . __lookupGetter__ ( { [ Symbol . toPrimitive ] : x => { await ; } } ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:ExpressionStatement[0,0].Evaluation) but got transpile-failure */
