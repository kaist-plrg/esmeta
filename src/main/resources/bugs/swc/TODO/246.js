"use strict";
class x { set [ 1n . #x . #x ^= 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:MemberExpression[7,0].Evaluation) but got throw-error: ReferenceError(unnamed:63: ReferenceError: _ref is not defined) */
