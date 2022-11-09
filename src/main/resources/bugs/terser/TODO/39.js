"use strict";
[ ... function * ( ) { yield x ; } ( ) ] ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(Some(GetValue ((step 3, 4:57-92))<SYNTAX>:YieldExpression[1,0].Evaluation)) but got normal */
