"use strict";
Array . prototype . reduceRight . call ( [ 0 ] , x => await , 0 ) ; 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(Some(GetValue ((step 3, 4:57-92))<SYNTAX>:ExpressionBody[0,0].Evaluation)) but got transpile-failure */
