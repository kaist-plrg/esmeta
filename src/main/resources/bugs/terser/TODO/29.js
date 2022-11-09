"use strict";
[ ... function * ( ) { yield * 0 ; } ( ) ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(Call ((step 2, 3:43-73))<SYNTAX>:YieldExpression[2,0].Evaluation)) but got normal */
