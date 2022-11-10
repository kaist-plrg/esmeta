"use strict";
class x { [ { [ 0 ] : x = 0 . x . x } ] ; } 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:AssignmentExpression[4,0].Evaluation) but got throw-error: ReferenceError(Exception: ReferenceError: Cannot access uninitialized variable.) */
