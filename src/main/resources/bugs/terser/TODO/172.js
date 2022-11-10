"use strict";
~ { [ Symbol . toPrimitive ] : x => this } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:UnaryExpression[6,0].Evaluation) but got normal */
