"use strict";
[ ... function * ( ) { yield * 0 ( ) ; } ( ) ] ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(EvaluateCall ((step 4, 12:45-75))<SYNTAX>:CallExpression[0,0].Evaluation)) but got normal */
