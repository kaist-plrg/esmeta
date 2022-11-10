"use strict";
class x { static #x = #x in super . x >> 0 . #x ; } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:ShiftExpression[2,0].Evaluation) but got transpile-failure */
