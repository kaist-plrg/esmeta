"use strict";
; [ 0n ] [ 0 ] >>>= 1n ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(BigInt::unsignedRightShift ((step 1, 2:16-46))<SYNTAX>:AssignmentExpression[5,0].Evaluation) but got normal */
