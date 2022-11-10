"use strict";
class x { set [ function ( ) { } ( ) . #x &&= 0 ] ( x ) { } get #x ( ) { } } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:AssignmentExpression[6,0].Evaluation) but got throw-error: ReferenceError(unnamed:63: ReferenceError: _ref is not defined) */
