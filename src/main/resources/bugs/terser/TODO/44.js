"use strict";
class x { set [ #x in 0 . #x >> 0 ?? 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:ShiftExpression[2,0].Evaluation) but got transpile-failure */
