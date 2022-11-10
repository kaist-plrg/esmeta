"use strict";
class x { static #x = #x in x >>> 0 % 0 . #x ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:MultiplicativeExpression[1,0].Evaluation) but got transpile-failure */
