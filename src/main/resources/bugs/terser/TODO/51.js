"use strict";
[ { [ Symbol . toPrimitive ] : x => new 0 } ] [ 0 ] == 0 ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(EvaluateNew ((step 5, 7:60-90))<SYNTAX>:NewExpression[1,0].Evaluation) but got normal */
