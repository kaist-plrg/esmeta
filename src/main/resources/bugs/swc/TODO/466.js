"use strict";
class x { set [ await . #x ^= 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-AWAIT-FAIL
[Exit Tag Mismatch]
 > Expected throw-error: ReferenceError(GetValue ((step 3, 4:57-92))<SYNTAX>:MemberExpression[7,0].Evaluation) but got transpile-failure */
