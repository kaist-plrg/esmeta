"use strict";
{ x = `${ 0 }` [ 0 ] ++ ?? 0 ; let x ; } ; 

/* TAG: NEW-YET
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(PutValue ((step 5.d, 15:72-102))<SYNTAX>:UpdateExpression[1,0].Evaluation) but got throw-error: ReferenceError(Exception: ReferenceError: Cannot access uninitialized variable.) */
