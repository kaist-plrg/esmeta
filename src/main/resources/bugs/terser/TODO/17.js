"use strict";
0 > { [ Symbol . toPrimitive ] : x => [ ] } / 0 ; 

/* TAG: NEW-YET-TRS-REMOVE-TYP-ERR
[Exit Tag Mismatch]
 > Expected throw-error: TypeError(ToPrimitive ((step 1.b.vi, 12:16-46))<SYNTAX>:MultiplicativeExpression[1,0].Evaluation) but got normal */
