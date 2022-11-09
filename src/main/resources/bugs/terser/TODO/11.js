"use strict";
+ { [ Symbol . toPrimitive ] : x => 0n } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToNumber ((step 7, 10:44-74))<SYNTAX>:UnaryExpression[4,0].Evaluation) but got normal */
