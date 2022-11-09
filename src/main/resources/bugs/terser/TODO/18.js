"use strict";
[ ... function * ( ) { yield * function * ( ) { x ( 0 ) ; } ( ) ; } ( ) ] ; 

/* TAG: NEW-YET-REMOVED-REF-ERR
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(Some(GetValue ((step 3, 4:57-92))<SYNTAX>:CallExpression[0,0].Evaluation)) but got normal */
