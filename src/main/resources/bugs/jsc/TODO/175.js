"use strict";
{ x = new null [ 0 ] ( ) ( ) [ 0 ] ++ ?? 0 ; let x ; } ; 

/* TAG: NEW-BECOME-CONSTRUCTABLE
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToObject<SYNTAX>:MemberExpression[6,0].Evaluation) but got throw-error: ReferenceError(Exception: ReferenceError: Cannot access uninitialized variable.) */
