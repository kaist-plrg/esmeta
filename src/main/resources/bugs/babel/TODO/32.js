"use strict";
class x { static #x = #x in x ?. #x . #x ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(Some(PrivateGet ((step 2, 3:35-65))<SYNTAX>:OptionalChain[9,0].ChainEvaluation)) but got throw-error: ReferenceError(unnamed:21: ReferenceError: x is not defined - temporal dead zone) */
