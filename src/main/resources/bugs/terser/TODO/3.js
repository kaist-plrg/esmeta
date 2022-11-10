"use strict";
class x { set [ #x in '' ?. [ 0 % 0 ] ?? 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(RelationalExpression[7,0].Evaluation ((step 4, 5:43-73))<SYNTAX>:RelationalExpression[7,0].Evaluation) but got transpile-failure */
