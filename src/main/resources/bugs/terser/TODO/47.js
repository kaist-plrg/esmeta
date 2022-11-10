"use strict";
class x { set [ #x in await ?? 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-PRIVATE-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:RelationalExpression[7,0].Evaluation) but got transpile-failure */
