"use strict";
class x { static #x = #x in { x } ?. x ?. x ( ) ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(EvaluateCall ((step 4, 12:45-75))<SYNTAX>:OptionalChain[5,0].ChainEvaluation)) but got throw-error: ReferenceError(unnamed:13: ReferenceError: x is not defined - temporal dead zone) */
