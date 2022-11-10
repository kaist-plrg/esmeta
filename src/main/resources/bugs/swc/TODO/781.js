"use strict";
class x { static #x = 0 + 0 ?. #x ; } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:AdditiveExpression[1,0].Evaluation) but got normal */
