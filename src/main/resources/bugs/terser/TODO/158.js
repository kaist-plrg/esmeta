"use strict";
0 == { [ Symbol . toPrimitive ] : async x => 0 >> 0 + x } ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:EqualityExpression[1,0].Evaluation) but got normal */
