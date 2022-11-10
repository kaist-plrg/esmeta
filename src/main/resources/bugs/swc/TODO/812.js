"use strict";
( x => function ( ) { } ( ) [ await ] ^= 0 ) ( ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:CallExpression[4,0].Evaluation) but got transpile-failure */
