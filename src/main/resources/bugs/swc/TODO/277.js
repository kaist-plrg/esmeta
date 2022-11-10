"use strict";
for ( 1n . x in [ 0 ] ) ; 

/* TAG: NEW-SWC-STRICT-FOR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PutValue ((step 5.d, 15:72-102))<SYNTAX>:ForInOfStatement[0,0].ForInOfLoopEvaluation) but got throw-error: ReferenceError(unnamed:4: ReferenceError: ref is not defined) */
