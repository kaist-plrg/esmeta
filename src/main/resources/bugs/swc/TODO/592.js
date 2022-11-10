"use strict";
class x { set [ 0 . #x . x ^= 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PrivateGet ((step 2, 3:35-65))<SYNTAX>:MemberExpression[2,0].Evaluation) but got throw-error: ReferenceError(unnamed:60: ReferenceError: _ref is not defined) */
